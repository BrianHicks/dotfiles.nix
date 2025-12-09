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

    return (set(formulae) | set(casks), taps)


def main(args):
    have_formulae, have_taps = query_brew(args.homebrew_bin)

    want_formulae = set(args.formulae) if args.formulae else set()
    want_taps = set(args.taps) if args.taps else set()

    missing_formulae = want_formulae - have_formulae
    missing_taps = want_taps - have_taps

    extra_formulae = have_formulae - want_formulae
    extra_taps = have_taps - want_taps

    # Take care of taps, then formulae. Remove in reverse.
    if missing_taps:
        print(f"tapping {', '.join(missing_taps)}")
        subprocess.check_call([args.homebrew_bin, "tap", *missing_taps])
    if missing_formulae:
        print(f"installing {', '.join(missing_formulae)}")
        subprocess.check_call([args.homebrew_bin, "install", *missing_formulae])

    if extra_formulae:
        print(f"uninstalling {', '.join(extra_formulae)}")
        subprocess.check_call([args.homebrew_bin, "uninstall", *extra_formulae])
    if extra_taps:
        print(f"untapping {', '.join(extra_taps)}")
        subprocess.check_call([args.homebrew_bin, "untap", *extra_taps])


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--taps", nargs="*")
    parser.add_argument("--formulae", nargs="*")
    parser.add_argument("--homebrew-bin", default="/opt/homebrew/bin/brew")
    main(parser.parse_args())
