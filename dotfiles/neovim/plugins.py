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
    def __init__(self, data):
        self.data = data

    def __str__(self):
        return '{} = {};'.format(self.attr(), self.expression())

    def owner(self):
        return self.data['owner']

    def repo(self):
        return self.data['repo']

    def attr(self):
        return '{}.{}'.format(
            escape(self.owner()),
            escape(self.repo()),
        )

    def rev(self):
        if self.data.get('rev', None) is None:
            print('determining latest hash for {}'.format(self.attr()), file=sys.stderr)
            with urlopen('https://api.github.com/repos/{}/{}/commits'.format(self.owner(), self.repo())) as resp:
                self.data['rev'] = json.load(resp)[0]["sha"]

        return self.data['rev']

    def sha256(self):
        if self.data.get('sha256', None) is None:
            print('prefetching sha256 for {}'.format(self.attr()), file=sys.stderr)
            self.data['sha256'] = subprocess.check_output([
                'nix-prefetch-url',
                '--unpack',
                'https://github.com/{}/{}/archive/{}.tar.gz'.format(
                    self.owner(),
                    self.repo(),
                    self.rev(),
                ),
            ]).decode('utf-8').strip()

        return self.data['sha256']

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

    with open(args.packages, 'r') as fh:
        for package in json.load(fh):
            if package['type'] == 'github':
                packages.append(GithubPackage(package))
            else:
                print("I don't know how to resolve a {} plugin".format(package['type']), file=sys.stderr)
                return 1

    packages.sort(key=lambda pkg: pkg.attr())

    print('{ pkgs, ... }:\n\n{\n')
    for package in packages:
        print('resolving {}'.format(package.attr()), file=sys.stderr)
        print(textwrap.indent(str(package), '  ') + '\n', file=sys.stdout)
    print('}')

    with open(args.packages, 'w') as fh:
        json.dump(
            [ pkg.data for pkg in packages ],
            fh,
            indent=2,
        )

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('packages')
    sys.exit(main(parser.parse_args()))
