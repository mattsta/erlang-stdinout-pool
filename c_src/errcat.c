#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

int is_open(int fd) {
    errno = 0;
    fcntl(fd, F_GETFD);
    return errno != EBADF;
}

int main(int argc, char **argv) {
    char buf[BUFSIZ];
    ssize_t count;

    do {
        count = read(STDIN_FILENO, buf, BUFSIZ);
    } while (count == -1 && errno == EINTR);

    if (count == -1) {
        perror("read");
        exit(1);
    }

    do {
        buf[count] = 0;
        fputs(buf, stderr);
        count = read(STDIN_FILENO, buf, BUFSIZ);
    } while (count > 0 && is_open(STDIN_FILENO));

    exit(0);
}