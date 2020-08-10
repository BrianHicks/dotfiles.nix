# This file has been generated by node2nix 1.8.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {
    "@types/chroma-js-1.4.3" = {
      name = "_at_types_slash_chroma-js";
      packageName = "@types/chroma-js";
      version = "1.4.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/chroma-js/-/chroma-js-1.4.3.tgz";
        sha512 = "m33zg9cRLtuaUSzlbMrr7iLIKNzrD4+M6Unt5+9mCu4BhR5NwnRjVKblINCwzcBXooukIgld8DtEncP8qpvbNg==";
      };
    };
    "ayu-7.3.1" = {
      name = "ayu";
      packageName = "ayu";
      version = "7.3.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/ayu/-/ayu-7.3.1.tgz";
        sha512 = "tflnLFex6AdRBsZONyznXVh3JYLzpkD+i5ZrzGbjddkWWhRCGDq3H3qAuk1m1bafo0u6qh+BANg/aE+E2KhzvA==";
      };
    };
    "chroma-js-2.1.0" = {
      name = "chroma-js";
      packageName = "chroma-js";
      version = "2.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/chroma-js/-/chroma-js-2.1.0.tgz";
        sha512 = "uiRdh4ZZy+UTPSrAdp8hqEdVb1EllLtTHOt5TMaOjJUvi+O54/83Fc5K2ld1P+TJX+dw5B+8/sCgzI6eaur/lg==";
      };
    };
    "cross-env-6.0.3" = {
      name = "cross-env";
      packageName = "cross-env";
      version = "6.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/cross-env/-/cross-env-6.0.3.tgz";
        sha512 = "+KqxF6LCvfhWvADcDPqo64yVIB31gv/jQulX2NGzKS/g3GEVz6/pt4wjHFtFWsHMddebWD/sDthJemzM4MaAag==";
      };
    };
    "cross-spawn-7.0.3" = {
      name = "cross-spawn";
      packageName = "cross-spawn";
      version = "7.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/cross-spawn/-/cross-spawn-7.0.3.tgz";
        sha512 = "iRDPJKUPVEND7dHPO8rkbOnPpyDygcDFtWjpeWNCgy8WP2rXcxXL8TskReQl6OrB2G7+UJrags1q15Fudc7G6w==";
      };
    };
    "isexe-2.0.0" = {
      name = "isexe";
      packageName = "isexe";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/isexe/-/isexe-2.0.0.tgz";
        sha1 = "e8fbf374dc556ff8947a10dcb0572d633f2cfa10";
      };
    };
    "nonenumerable-1.1.1" = {
      name = "nonenumerable";
      packageName = "nonenumerable";
      version = "1.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/nonenumerable/-/nonenumerable-1.1.1.tgz";
        sha512 = "ptUD9w9D8WqW6fuJJkZNCImkf+0vdbgUTbRK3i7jsy3olqtH96hYE6Q/S3Tx9NWbcB/ocAjYshXCAUP0lZ9B4Q==";
      };
    };
    "path-key-3.1.1" = {
      name = "path-key";
      packageName = "path-key";
      version = "3.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/path-key/-/path-key-3.1.1.tgz";
        sha512 = "ojmeN0qd+y0jszEtoY48r0Peq5dwMEkIlCOu6Q5f41lfkswXuKtYrhgoTpLnyIcHm24Uhqx+5Tqm2InSwLhE6Q==";
      };
    };
    "shebang-command-2.0.0" = {
      name = "shebang-command";
      packageName = "shebang-command";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/shebang-command/-/shebang-command-2.0.0.tgz";
        sha512 = "kHxr2zZpYtdmrN1qDjrrX/Z1rR1kG8Dx+gkpK1G4eXmvXswmcE1hTWBWYUzlraYw1/yZp6YuDY77YtvbN0dmDA==";
      };
    };
    "shebang-regex-3.0.0" = {
      name = "shebang-regex";
      packageName = "shebang-regex";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/shebang-regex/-/shebang-regex-3.0.0.tgz";
        sha512 = "7++dFhtcx3353uBaq8DDR4NuxBetBzC7ZQOhmTQInHEd6bSrXdiEyzCvG07Z44UYdLShWUyXt5M/yhz8ekcb1A==";
      };
    };
    "which-2.0.2" = {
      name = "which";
      packageName = "which";
      version = "2.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/which/-/which-2.0.2.tgz";
        sha512 = "BLI3Tl1TW3Pvl70l3yq3Y64i+awpwXqsGBYWkkqMtnbXgrMD+yj7rhW0kuEDxzJaYXGjEW5ogapKNMEKNMjibA==";
      };
    };
  };
  args = {
    name = "kak-ayu";
    packageName = "kak-ayu";
    version = "1.0.0";
    src = ./.;
    dependencies = [
      sources."@types/chroma-js-1.4.3"
      sources."ayu-7.3.1"
      sources."chroma-js-2.1.0"
      sources."cross-env-6.0.3"
      sources."cross-spawn-7.0.3"
      sources."isexe-2.0.0"
      sources."nonenumerable-1.1.1"
      sources."path-key-3.1.1"
      sources."shebang-command-2.0.0"
      sources."shebang-regex-3.0.0"
      sources."which-2.0.2"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "";
      license = "ISC";
    };
    production = true;
    bypassCache = true;
    reconstructLock = false;
  };
in
{
  args = args;
  sources = sources;
  tarball = nodeEnv.buildNodeSourceDist args;
  package = nodeEnv.buildNodePackage args;
  shell = nodeEnv.buildNodeShell args;
}