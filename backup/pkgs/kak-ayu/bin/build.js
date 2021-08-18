#!/usr/bin/env node
let ayu = require('ayu');
let fs = require('fs');

function rgb(color) {
  return `rgba:${color.hex('rgba').slice(1)}`
}

[
  {name: 'dark', colors: ayu.dark},
  {name: 'light', colors: ayu.light},
  {name: 'mirage', colors: ayu.mirage}
].map(function(theme) {
  let colors = theme.colors;

  fs.writeFileSync(
    `ayu-${theme.name}.kak`,
    `\
# Code highlighting
face global value     ${rgb(colors.syntax.markup)}
face global type      ${rgb(colors.syntax.entity)}+b
face global variable  ${rgb(colors.common.fg)}
face global module    ${rgb(colors.syntax.entity)}+b
face global function  ${rgb(colors.syntax.func)}
face global string    ${rgb(colors.syntax.string)}
face global keyword   ${rgb(colors.syntax.keyword)}
face global operator  ${rgb(colors.syntax.operator)}
face global attribute ${rgb(colors.syntax.operator)}
face global comment   ${rgb(colors.syntax.comment.brighten(0.5))}+i
face global meta      ${rgb(colors.syntax.special)}
face global builtin   ${rgb(colors.syntax.tag)}

# Markdown highlighting
face global title     ${rgb(colors.syntax.tag)}+b
face global header    ${rgb(colors.syntax.tag)}
face global bold      ${rgb(colors.common.fg)}+b
face global italic    ${rgb(colors.common.fg)}+i
face global mono      ${rgb(colors.syntax.string)}
face global block     default
face global link      ${rgb(colors.syntax.markup)}
face global bullet    ${rgb(colors.syntax.markup)}
face global list      ${rgb(colors.syntax.markup)}

# LSP
face global DiagnosticError   ${rgb(colors.syntax.error)}
face global DiagnosticWarning ${rgb(colors.syntax.error.fade(0.3))}

# builtin
face global Default ${rgb(colors.common.fg)},${rgb(colors.common.bg)}
face global Error   ${rgb(colors.syntax.error)}

face global PrimarySelection   default,${rgb(colors.ui.selection.bg.brighten(0.3))}
face global SecondarySelection default,${rgb(colors.ui.selection.inactive)}
face global PrimaryCursor      ${rgb(colors.common.bg)},${rgb(colors.common.accent)}
face global SecondaryCursor    ${rgb(colors.common.bg)},${rgb(colors.common.accent.fade(0.3))}
face global PrimaryCursorEol   ${rgb(colors.common.bg)},${rgb(colors.common.accent.fade(0.5))}
face global SecondaryCursorEol ${rgb(colors.common.bg)},${rgb(colors.common.accent.fade(0.8))}
face global InactiveCursor     default

face global MenuForeground ${rgb(colors.common.fg)},${rgb(colors.ui.selection.bg)}+b
face global MenuBackground ${rgb(colors.common.fg)},${rgb(colors.ui.selection.inactive)}
face global MenuInfo       ${rgb(colors.common.fg)}+d

face global Information ${rgb(colors.common.bg)},${rgb(colors.common.fg)}

face global StatusLine      default,${rgb(colors.ui.selection.border)}
face global StatusLineMode  ${rgb(colors.common.bg)},${rgb(colors.common.accent)}
face global StatusLineInfo  ${rgb(colors.ui.selection.border)},${rgb(colors.common.fg)}
face global StatusLineValue ${rgb(colors.common.bg)},${rgb(colors.common.accent)}
face global Prompt          ${rgb(colors.common.accent)}

# BufferPadding
face global LineNumbers        ${rgb(colors.common.fg)}
face global LineNumberCursor   ${rgb(colors.common.fg)}+b
face global LineNumbersWrapped ${rgb(colors.common.ui)}

face global MatchingChar default,${rgb(colors.ui.selection.bg)}+b

# Whitespace
# WrapMarker`
  )
})
