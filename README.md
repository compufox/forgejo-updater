# forgejo-updater
### _a. fox_

a tool to help keep forgejo up to date on your system

## Running

`./fupdater --help`:

```
Usage: fupdater [-h|--help] [-f|--force] [-d|--download] [-o|--output FILE]
                [-u|--update] [-r|--restart] [--arch] [--restart-command COMMAND] [--version]
                [-v|--verbose] [--run-as-root]

Available options:
  -h, --help                prints this help
  -f, --force               forces the updater to download the latest version
  -d, --download            downloads the new Forgejo version
  -o, --output FILE         specify the full file path to download the new release
  -u, --update              replaces the forgejo binary with new version automatically
  -r, --restart             restarts the forgejo systemd unit after updating
  --arch                    specifies the arch to download (default: linux-amd64)
  --restart-command COMMAND
                            pass the command to restart the forgejo process (default: sudo systemctl restart forgejo)
  --version                 prints the program's version
  -v, --verbose             prints logging info
  --run-as-root             force binary to run as root
```

## Building

1. install [roswell](https://github.com/roswell/roswell)
2. `$ mkdir ~/common-lisp && git clone https://github.com/compufox/forgejo-updater ~/common-lisp/forgejo-updater`
3. `$ cd ~/common-lisp/forgejo-updater && make`

## License

MIT

