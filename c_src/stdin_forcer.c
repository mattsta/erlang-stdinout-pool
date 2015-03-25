#include <sys/wait.h>
#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>
#include <fcntl.h>

#define PARENT_READ readpipe[0]
#define CHILD_WRITE readpipe[1]
#define CHILD_READ writepipe[0]
#define PARENT_WRITE writepipe[1]
#define PARENT_ERROR errorpipe[0]
#define CHILD_ERROR errorpipe[1]

const unsigned char PU1 = 145;  /* ASCII & UTF-8 control character: 145 | 0x91 | PU1 | Reserved for private use. */

int dup2close(int oldfd, int newfd) {
  while ((dup2(oldfd, newfd) == -1) && (errno == EINTR)) {}
  return close(oldfd);
}

int is_open(int fd) {
    errno = 0;
    fcntl(fd, F_GETFD);
    return errno != EBADF;
}

void toSTDOUT(int fd, const char *firstByte) {
  char buf[BUFSIZ];
  ssize_t count;

  do { count = read(fd, buf, BUFSIZ); }
  while ( count == -1 && errno == EINTR );

  if (count == -1) {
      perror("read");
      exit(1);
  }

  if (firstByte && count > 0)
    write(STDOUT_FILENO, firstByte, 1); /* Write first byte */

  do {
    write(STDOUT_FILENO, buf, count); /* Vomit forth our output on STDOUT */
    count = read(fd, buf, BUFSIZ);
  }
  while (count > 0 && is_open(fd));
}

int main(int argc, char *argv[]) {
    int readpipe[2], writepipe[2], errorpipe[2];
    int unused __attribute__((unused));
    pid_t cpid;

    assert(1 < argc && argc < 64);

    if (pipe(readpipe) == -1 || pipe(writepipe) == -1 || pipe(errorpipe) == -1) {
        perror("pipe");
        exit(EXIT_FAILURE);
    }

    cpid = fork();
    if (cpid == -1) {
        perror("fork");
        exit(EXIT_FAILURE);
    }

    if (cpid == 0) {
        /* Forked Child with STDIN forwarding */
        char *cmd = argv[1];
        char *exec_args[64] = {0};
        int i;

        for (i = 0; i < argc - 1; i++) {
            /* args to stdin_forcer are the program and optional args to exec.
               Here we copy pointers pointing to strings of cmd/args.
               exec_args is indexed one lower than argv. */
            exec_args[i] = argv[i + 1];
        }

        close(PARENT_READ); /* We aren't the parent. Decrement fd refcounts. */
        close(PARENT_WRITE);
	close(PARENT_ERROR);

        /* CHILD_READ  = STDIN  to the exec'd process.
           CHILD_WRITE = STDOUT to the exec'd process.
           CHILD_ERROR = STDERR to the exec'd process. */
        if (!dup2close(CHILD_READ, STDIN_FILENO) &&
            dup2close(CHILD_WRITE, STDOUT_FILENO) &&
	    dup2close(CHILD_ERROR, STDERR_FILENO)) {
            perror("dup2 or close");
            _exit(EXIT_FAILURE);
        }

        /* At this point, the execv'd program's STDIN and STDOUT are the pipe */
        if (execv(cmd, exec_args) == -1) {
            perror("execve");
        }
        _exit(EXIT_FAILURE); /* Silence a warning */
    } else {
        /* Original Parent Process */
	char buf;

        close(CHILD_READ); /* We aren't the child.  Close its read/write. */
        close(CHILD_WRITE);
	close(CHILD_ERROR);
        /* We should catch the child's exit signal if it dies before we send
         * STDIN*/
        while (read(STDIN_FILENO, &buf, 1) > 0 && buf != 0x0) {
            unused = write(PARENT_WRITE, &buf, 1);
        }
        close(PARENT_WRITE); /* closing PARENT_WRITE sends EOF to CHILD_READ */

	toSTDOUT(PARENT_READ, 0);
	toSTDOUT(PARENT_ERROR, (char*)&PU1);

	close(PARENT_READ); /* done reading from writepipe */
	close(PARENT_ERROR); /* done reading from errorpipe */

        wait(NULL);          /* Wait for child to exit */

        exit(EXIT_SUCCESS); /* This was a triumph */
    }
}
