#!/usr/bin/env python
import argparse
import json
import subprocess
import sys
import textwrap
from urllib.parse import urlparse
from urllib.request import urlopen

def escape(input_):
    if '.' in input_:
        return '"{}"'.format(input_)
    return input_


class GithubPackage:
    def __init__(self, url):
        self.url = url
        self._rev = url.fragment or None
        self._sha256 = None

    def __str__(self):
        return '{} = {};'.format(self.attr(), self.expression())

    def owner(self):
        return self.url.path.split('/')[1]

    def repo(self):
        return self.url.path.split('/')[2]

    def attr(self):
        return '{}.{}'.format(
            escape(self.owner()),
            escape(self.repo()),
        )

    def rev(self):
        if self._rev is None:
            print('determining latest hash for {}'.format(self.attr()), file=sys.stderr)
            with urlopen('https://api.github.com/repos/{}/{}/commits'.format(self.owner(), self.repo())) as resp:
                self._rev = json.load(resp)[0]["sha"]

        return self._rev

    def sha256(self):
        if self._sha256 is None:
            print('prefetching sha256 for {}'.format(self.attr()), file=sys.stderr)
            self._sha256 = subprocess.check_output([
                'nix-prefetch-url',
                '--unpack',
                'https://github.com/{}/{}/archive/{}.tar.gz'.format(
                    self.owner(),
                    self.repo(),
                    self.rev(),
                ),
            ]).decode('utf-8').strip()

        return self._sha256

    def expression(self):
        return textwrap.dedent('''\
            pkgs.vimUtils.buildVimPlugin {
              name = "%s";
              src = pkgs.fetchFromGitHub {
                owner = "%s";
                repo = "%s";
                rev = "%s";
                sha256 = "%s";
              };
            }
        ''') % (
            self.repo(),
            self.owner(),
            self.repo(),
            self.rev(),
            self.sha256(),
        )

def main(args):
    packages = []

    for package in json.load(args.packages):
        url = urlparse(package)
        if 'github.com' in url.netloc:
            packages.append(GithubPackage(url))
        else:
            print("I don't know how to resolve {}".format(package), file=sys.stderr)
            return 1

    print('{ pkgs, ... }:\n\n{\n')
    for package in packages:
        print('resolving {}'.format(package.attr()), file=sys.stderr)
        print(textwrap.indent(str(package), '  ') + '\n', file=sys.stdout)
    print('}')

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('packages', type=open)
    sys.exit(main(parser.parse_args()))
