#!/usr/bin/env python3
import argparse
import subprocess


def query_brew(homebrew_bin: str):
    formulae = set(
        subprocess.check_output([homebrew_bin, "list", "--formulae", "-1"])
        .decode()
        .strip()
        .splitlines()
    )

    casks = set(
        subprocess.check_output([homebrew_bin, "list", "--casks", "-1"])
        .decode()
        .strip()
        .splitlines()
    )

    taps = set(
        subprocess.check_output([homebrew_bin, "tap"]).decode().strip().splitlines()
    )

    return (formulae, casks, taps)


def main(args):
    have_formulae, have_casks, have_taps = query_brew(args.homebrew_bin)

    want_formulae = set(args.formulae) if args.formulae else set()
    want_casks = set(args.casks) if args.casks else set()
    want_taps = set(args.taps) if args.taps else set()

    missing_formulae = want_formulae - have_formulae
    missing_casks = want_casks - have_casks
    missing_taps = want_taps - have_taps

    extra_formulae = have_formulae - want_formulae
    extra_casks = have_casks - want_casks
    extra_taps = have_taps - want_taps

    # Take care of taps, then casks, then formulae. Then remove in reverse.
    if missing_taps:
        subprocess.check_call([args.homebrew_bin, "tap", *missing_taps])
    if missing_formulae:
        subprocess.check_call([args.homebrew_bin, "install", *missing_formulae])
    if missing_casks:
        subprocess.check_call([args.homebrew_bin, "install", "--casks", *missing_casks])

    if extra_casks:
        subprocess.check_call([args.homebrew_bin, "uninstall", *extra_casks])
    if extra_formulae:
        subprocess.check_call([args.homebrew_bin, "uninstall", *extra_formulae])
    if extra_taps:
        subprocess.check_call([args.homebrew_bin, "untap", *extra_taps])


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--taps", nargs="*")
    parser.add_argument("--formulae", nargs="*")
    parser.add_argument("--casks", nargs="*")
    parser.add_argument("--homebrew-bin", default="/opt/homebrew/bin/brew")
    main(parser.parse_args())
