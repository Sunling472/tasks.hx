/*
 * hx-task -- process runner for the helix + steel tasks plugin.
 *
 * Owns the process group (and optionally a pty) so the plugin can kill a whole
 * task tree, forward Ctrl-C, and get a reliable exit code without touching
 * Steel's process API.
 *
 * usage:
 *   hx-task [--pipe] [--cols N] [--rows N] -- <cmd> [args...]
 *
 * stdout (to the plugin), one event per line:
 *   L <text>   a completed output line
 *   R <text>   replace the previously displayed line (progress bars: "10%\r20%")
 *   X <code>   child exited with <code>; "X sig:<n>" when killed by a signal
 *
 * stdin (from the plugin), single bytes:
 *   t   SIGTERM the process group, escalate to SIGKILL after 400ms
 *   k   SIGKILL the process group now
 *   i   write 0x03 (Ctrl-C) to the pty
 *   q   same as t, then exit once the child is gone
 *   EOF same as q (the plugin went away)
 */

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pty.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#define KILL_GRACE_MS 400

static int out_fd = -1;
static int out_eof = 0;
static pid_t child_pid = -1;
static int use_pty = 1;

static char *lbuf = NULL;
static size_t lcap = 0, llen = 0;
static int pending_cr = 0;

static long kill_at = -1;

static long now_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000L + ts.tv_nsec / 1000000L;
}

static void kill_group(int sig) {
  if (child_pid <= 0) {
    return;
  }
  if (kill(-child_pid, sig) != 0) {
    kill(child_pid, sig);
  }
}

static void write_all(int fd, const char *buf, size_t len) {
  while (len > 0) {
    ssize_t n = write(fd, buf, len);
    if (n > 0) {
      buf += n;
      len -= (size_t)n;
      continue;
    }
    if (n < 0 && errno == EINTR) {
      continue;
    }
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
      struct pollfd p = {.fd = fd, .events = POLLOUT};
      if (poll(&p, 1, 100) > 0) {
        continue;
      }
    }
    /* the plugin is gone: take the task down with us */
    kill_group(SIGKILL);
    _exit(0);
  }
}

static void emit(int kind, const char *data, size_t len) {
  char hdr[8];
  hdr[0] = (char)kind;
  hdr[1] = ' ';
  write_all(1, hdr, 2);
  if (len > 0) {
    write_all(1, data, len);
  }
  write_all(1, "\n", 1);
}

static void push_byte(char c) {
  if (llen + 2 >= lcap) {
    size_t nc = lcap ? lcap * 2 : 512;
    char *nb = (char *)realloc(lbuf, nc);
    if (!nb) {
      return;
    }
    lbuf = nb;
    lcap = nc;
  }
  lbuf[llen++] = c;
}

static void emit_line(int kind) {
  emit(kind, lbuf ? lbuf : "", llen);
  llen = 0;
}

static void feed(const char *data, size_t len) {
  for (size_t i = 0; i < len; i++) {
    char c = data[i];
    if (pending_cr) {
      if (c == '\n') { /* CRLF: the line is complete */
        pending_cr = 0;
        emit_line('L');
        continue;
      }
      pending_cr = 0;
      emit_line('R');
    }
    if (c == '\r') {
      pending_cr = 1;
      continue;
    }
    if (c == '\n') {
      emit_line('L');
      continue;
    }
    push_byte(c);
  }
}

static void handle_stdin(void) {
  char buf[256];
  ssize_t n = read(0, buf, sizeof buf);
  if (n < 0) {
    if (errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK) {
      return;
    }
    n = 0;
  }
  if (n == 0) { /* plugin closed the pipe */
    kill_group(SIGTERM);
    if (kill_at < 0) {
      kill_at = now_ms() + KILL_GRACE_MS;
    }
    return;
  }
  for (ssize_t i = 0; i < n; i++) {
    switch (buf[i]) {
      case 't':
        kill_group(SIGTERM);
        if (kill_at < 0) {
          kill_at = now_ms() + KILL_GRACE_MS;
        }
        break;
      case 'k':
        kill_group(SIGKILL);
        kill_at = -1;
        break;
      case 'i':
        if (use_pty && out_fd >= 0) {
          char intr = 0x03;
          ssize_t w = write(out_fd, &intr, 1);
          (void)w;
        }
        break;
      case 'q':
        kill_group(SIGTERM);
        if (kill_at < 0) {
          kill_at = now_ms() + KILL_GRACE_MS;
        }
        break;
      default:
        break;
    }
  }
}

static void handle_output(void) {
  char buf[65536];
  for (;;) {
    ssize_t n = read(out_fd, buf, sizeof buf);
    if (n > 0) {
      feed(buf, (size_t)n);
      continue;
    }
    if (n < 0 && errno == EINTR) {
      continue;
    }
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
      return;
    }
    if (n == 0 || (n < 0 && errno == EIO)) {
      out_eof = 1;
    }
    return;
  }
}

static int spawn_child(int argc, char **argv, int argi, int cols, int rows) {
  if (use_pty) {
    struct winsize ws;
    memset(&ws, 0, sizeof ws);
    ws.ws_col = (unsigned short)cols;
    ws.ws_row = (unsigned short)rows;
    child_pid = forkpty(&out_fd, NULL, NULL, &ws);
    if (child_pid < 0) {
      perror("hx-task: forkpty");
      return -1;
    }
    if (child_pid == 0) {
      char num[16];
      setenv("TERM", "xterm-256color", 1);
      setenv("COLORTERM", "truecolor", 1);
      snprintf(num, sizeof num, "%d", cols);
      setenv("COLUMNS", num, 1);
      snprintf(num, sizeof num, "%d", rows);
      setenv("LINES", num, 1);
      execvp(argv[argi], &argv[argi]);
      _exit(127);
    }
  } else {
    int fds[2];
    if (pipe(fds) != 0) {
      perror("hx-task: pipe");
      return -1;
    }
    child_pid = fork();
    if (child_pid < 0) {
      perror("hx-task: fork");
      return -1;
    }
    if (child_pid == 0) {
      setsid();
      dup2(fds[1], STDOUT_FILENO);
      dup2(fds[1], STDERR_FILENO);
      close(fds[0]);
      close(fds[1]);
      setenv("TERM", "dumb", 1);
      execvp(argv[argi], &argv[argi]);
      _exit(127);
    }
    close(fds[1]);
    out_fd = fds[0];
  }
  (void)argc;
  return 0;
}

int main(int argc, char **argv) {
  int cols = 120;
  int rows = 40;
  int i = 1;

  signal(SIGPIPE, SIG_IGN);

  for (; i < argc; i++) {
    if (!strcmp(argv[i], "--pipe")) {
      use_pty = 0;
    } else if (!strcmp(argv[i], "--cols") && i + 1 < argc) {
      cols = atoi(argv[++i]);
    } else if (!strcmp(argv[i], "--rows") && i + 1 < argc) {
      rows = atoi(argv[++i]);
    } else if (!strcmp(argv[i], "--")) {
      i++;
      break;
    } else {
      break;
    }
  }
  if (i >= argc) {
    fprintf(stderr, "usage: hx-task [--pipe] [--cols N] [--rows N] -- <cmd> [args...]\n");
    return 2;
  }
  if (cols < 20) {
    cols = 20;
  }
  if (cols > 500) {
    cols = 500;
  }
  if (rows < 5) {
    rows = 5;
  }
  if (rows > 200) {
    rows = 200;
  }

  if (spawn_child(argc, argv, i, cols, rows) != 0) {
    return 1;
  }
  fcntl(out_fd, F_SETFL, fcntl(out_fd, F_GETFL) | O_NONBLOCK);
  fcntl(0, F_SETFL, fcntl(0, F_GETFL) | O_NONBLOCK);

  int status = 0;
  for (;;) {
    struct pollfd pfd[2];
    nfds_t nfds = 0;
    pfd[nfds].fd = 0;
    pfd[nfds].events = POLLIN;
    pfd[nfds].revents = 0;
    nfds++;
    if (!out_eof) {
      pfd[nfds].fd = out_fd;
      pfd[nfds].events = POLLIN;
      pfd[nfds].revents = 0;
      nfds++;
    }

    int timeout = 100;
    if (kill_at >= 0) {
      long d = kill_at - now_ms();
      if (d < 0) {
        d = 0;
      }
      if (d < timeout) {
        timeout = (int)d;
      }
    }

    int pr = poll(pfd, nfds, timeout);
    if (pr > 0) {
      if (pfd[0].revents & (POLLIN | POLLHUP | POLLERR)) {
        handle_stdin();
      }
      if (nfds > 1 && (pfd[1].revents & (POLLIN | POLLHUP | POLLERR))) {
        handle_output();
      }
    }

    if (kill_at >= 0 && now_ms() >= kill_at) {
      kill_group(SIGKILL);
      kill_at = -1;
    }

    pid_t r = waitpid(child_pid, &status, WNOHANG);
    if (r == child_pid) {
      /* the child may have left data in the pty/pipe */
      if (!out_eof) {
        for (int k = 0; k < 1000; k++) {
          char buf[65536];
          ssize_t n = read(out_fd, buf, sizeof buf);
          if (n > 0) {
            feed(buf, (size_t)n);
            continue;
          }
          if (n < 0 && errno == EINTR) {
            continue;
          }
          break;
        }
      }
      if (pending_cr) {
        pending_cr = 0;
        emit_line('R');
      }
      if (llen > 0) {
        emit_line('L');
      }
      break;
    }
  }

  char msg[64];
  if (WIFEXITED(status)) {
    snprintf(msg, sizeof msg, "X %d", WEXITSTATUS(status));
  } else if (WIFSIGNALED(status)) {
    snprintf(msg, sizeof msg, "X sig:%d", WTERMSIG(status));
  } else {
    snprintf(msg, sizeof msg, "X ?");
  }
  write_all(1, msg, strlen(msg));
  write_all(1, "\n", 1);
  return 0;
}
