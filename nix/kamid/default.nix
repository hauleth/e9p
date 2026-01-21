# SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
#
# SPDX-License-Identifier: Apache-2.0

{
  inputs,

  kamid,
  autoreconfHook,
  pkg-config,
  bison,
  stdenv,

  lib
}:

kamid.overrideAttrs (old: {
  version = "git";

  src = inputs.kamid;

  nativeBuildInputs = old.nativeBuildInputs ++ [
    autoreconfHook
    pkg-config
    bison
  ];

  patches = lib.optionals stdenv.isDarwin [
    ./macos.patch
  ];

  meta.package.platforms = old.meta.package.platforms ++ [
    "aarch64-darwin"
  ];
})
