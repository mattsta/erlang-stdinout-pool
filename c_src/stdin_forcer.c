#include <sys/wait.h>
#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>

#define PARENT_READ readpipe[0]
#define CHILD_WRITE readpipe[1]
#define CHILD_READ writepipe[0]
#define PARENT_WRITE writepipe[1]
#define PARENT_ERROR errorpipe[0]
#define CHILD_ERROR errorpipe[1]

/* Status bytes */
enum {
    /* ASCII & UTF-8 control character: 145 | 0x91 | PU1 | for private use. */
    SUCCESS_BYTE = 0x91,
    /* ASCII & UTF-8 control character: 146 | 0x92 | PU2 | for private use. */
    ERROR_BYTE = 0x92,
};

int dup2close(int oldfd, int newfd) {
    int dupResult;
    do {
        dupResult = dup2(oldfd, newfd);
    } while ((dupResult == -1) && (errno == EINTR));

    return close(oldfd);
}

void toSTDOUT(int fd, const char firstByte) {
    char buf[BUFSIZ];
    ssize_t count;

    do {
        count = read(fd, buf, BUFSIZ);
    } while (count == -1 && errno == EINTR);

    if (count == -1) {
        perror("read");
        exit(1);
    } else if (count > 0) {
        if (firstByte)
            write(STDOUT_FILENO, &firstByte, 1); /* Write first byte */

        do {
            write(STDOUT_FILENO, buf, count); /* write buffer to STDOUT */
            count = read(fd, buf, BUFSIZ);
        } while (count > 0);
    }
}

int main(int argc, char *argv[]) {
    int readpipe[2], writepipe[2], errorpipe[2];
    int unused __attribute__((unused));
    pid_t cpid;

    assert(1 < argc && argc < 64);

    if (pipe(readpipe) == -1 || pipe(writepipe) == -1 ||
        pipe(errorpipe) == -1) {
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
        if (dup2close(CHILD_READ, STDIN_FILENO) ||
            dup2close(CHILD_WRITE, STDOUT_FILENO) ||
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

        /* Read until 0 byte */
        /* TODO: Create a better length-prefixed protocol so we don't
         *       rely on a single end-of-stream byte markers. */
        while (read(STDIN_FILENO, &buf, 1) > 0 && buf != 0x0) {
            unused = write(PARENT_WRITE, &buf, 1);
        }
        close(PARENT_WRITE); /* closing PARENT_WRITE sends EOF to CHILD_READ */

        toSTDOUT(PARENT_READ, (uint8_t)SUCCESS_BYTE);
        toSTDOUT(PARENT_ERROR, (uint8_t)ERROR_BYTE);

        close(PARENT_READ);   /* done reading from writepipe */
        close(PARENT_ERROR);  /* done reading from errorpipe */
        close(STDOUT_FILENO); /* done writing to stdout */

        wait(NULL); /* Wait for child to exit */

        exit(EXIT_SUCCESS); /* This was a triumph */
    }
}
