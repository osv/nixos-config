{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.cli-apps.wine"
    "Wine"
    (with pkgs; [
      winePackages.unstable
      winetricks
      wine64Packages.unstable
    ])
    { derivative.homeDirectories = [ ".local/share/wineprefixes/default" ]; }
    {}
  ]

  [
    "nerv.opt.cli-apps.bpftrace"
    "sys call tracing tool (bpftrace-rw)"
    (with pkgs; [
      bpftrace
      (writeShellScriptBin "bpftrace-rw" ''

        BPFTRACE_MAX_STRLEN=90
        $${bpftrace}/bin/bpftrace -e '
        tracepoint:syscalls:sys_enter_openat
        {
            @open_filenames[pid] = str(args->filename);
        }

        tracepoint:syscalls:sys_exit_openat
        {
            if (args->ret > 0) {
                @files[pid, args->ret] = @open_filenames[pid];
            }
            delete(@open_filenames[pid]);
        }

        tracepoint:syscalls:sys_enter_read
        {
            @sizes[pid, args->fd] = args->count;
        }

        tracepoint:syscalls:sys_enter_write
        {
            @sizes[pid, args->fd] = args->count;
        }

        tracepoint:syscalls:sys_exit_read
        {
            $$filename = @files[pid, args->ret];
            if ($$filename != "") {
                // Green color for read
                printf("\033[32mREAD %s %d bytes by PID %d (%s)\033[0m\n", $$filename, @sizes[pid, args->ret], pid, comm);
                delete(@sizes[pid, args->ret]);
                        }
            }

        tracepoint:syscalls:sys_exit_write
        {
            $$filename = @files[pid, args->ret];
            if ($$filename != "") {
                // Red color for write
                printf("\033[31mWRITE %s %d bytes by PID %d (%s)\033[0m\n", $$filename, @sizes[pid, args->ret], pid, comm);
                delete(@sizes[pid, args->ret]);
                        }
            }

        tracepoint:syscalls:sys_exit_close
        {
            delete(@files[pid, args->ret]);
        }'
      '')
    ])
    {}
    {}
  ]

  [
    "nerv.opt.cli-apps.proton"
    "Proton caller utility"
    (with pkgs; [ proton-caller ])
    {}
    {}
  ]

  [
    "nerv.opt.cli-apps.wshowkeys"
    "Wayland keyboard input display utility"
    (with pkgs; [ wshowkeys ])
    {}
    {
      nerv.opt.user.extraGroups = [ "input" ];
    }
  ]
]
