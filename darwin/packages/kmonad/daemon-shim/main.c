/* Copyright (C) 2022 Matthew Toohey */
/* Source: <https://github.com/mtoohey31/infra/blob/e477bbf580358609a6c98721c403703d669fee41/pkgs/os-specific/darwin/kmonad-daemon-shim/main.c> */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

char *const kdv_client[] = {"Karabiner-DriverKit-VirtualHIDDeviceClient"};

int main(int argc, char *argv[]) {
  int r = fork();
  if (r < 0) {
    perror("kmonad-service-shim: fork");
    return 1;
  } else if (r == 0) {
    r = execvp(kdv_client[0], kdv_client);
  } else {
    // To give time for kdv-client to start
    sleep(1);
    r = execvp("kmonad", argv);
  };
  if (r < 0) {
    perror("kmonad-service-shim: execvp");
    return 1;
  }
}
