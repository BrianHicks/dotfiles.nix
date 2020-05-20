let ayu = require('ayu');
let fs = require('fs');

function rgb(color) {
  return `rgb:${color.hex('rgb').slice(1)}`
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
face global value     ${rgb(colors.syntax.constant)}
face global type      ${rgb(colors.syntax.tag)}+b
face global variable  ${rgb(colors.common.fg)}
face global module    ${rgb(colors.syntax.tag)}
face global function  ${rgb(colors.syntax.func)}
face global string    ${rgb(colors.syntax.string)}
face global keyword   ${rgb(colors.syntax.keyword)}
face global operator  ${rgb(colors.syntax.operator)}
face global attribute ${rgb(colors.syntax.keyword)}
face global comment   ${rgb(colors.syntax.comment)}
face global meta      ${rgb(colors.syntax.comment)}
face global builtin   ${rgb(colors.syntax.func)}+b

# Markdown highlighting
face global title     ${rgb(colors.syntax.func)}+b
face global header    ${rgb(colors.syntax.tag)}
face global bold      ${rgb(colors.common.fg)}+b
face global italic    ${rgb(colors.common.fg)}+i
face global mono      ${rgb(colors.common.fg)}
face global block     default
face global link      default
face global bullet    default
face global list      default

# builtin
face global Error              default,${rgb(colors.syntax.error)}
face global Information        ${rgb(colors.common.bg)},${rgb(colors.common.fg)}
face global LineNumberCursor   ${rgb(colors.ui.line)},${rgb(colors.common.accent)}
face global LineNumbers        ${rgb(colors.ui.guide.normal)},${rgb(colors.ui.gutter.normal)}
face global MatchingChar       default,${rgb(colors.common.bg)}
face global MenuBackground     default,${rgb(colors.ui.panel.bg)}
face global MenuForeground     ${rgb(colors.ui.panel.bg)},${rgb(colors.common.fg)}
face global MenuInfo           ${rgb(colors.common.fg)}
face global PrimaryCursor      ${rgb(colors.common.bg)},${rgb(colors.common.fg)}
face global PrimaryCursorEol   ${rgb(colors.common.bg)},${rgb(colors.common.fg)}
face global PrimarySelection   ${rgb(colors.common.fg)},${rgb(colors.ui.selection.bg)}
face global Prompt             ${rgb(colors.common.accent)}
face global SecondaryCursor    ${rgb(colors.common.bg)},${rgb(colors.common.fg)}
face global SecondaryCursorEol ${rgb(colors.common.bg)},${rgb(colors.common.fg)}
face global SecondarySelection ${rgb(colors.common.fg)},${rgb(colors.ui.selection.bg)}
face global StatusCursor       ${rgb(colors.common.bg)},${rgb(colors.common.fg)}
face global StatusLine         ${rgb(colors.common.fg)},${rgb(colors.ui.panel.bg)}
face global StatusLineInfo     ${rgb(colors.syntax.tag)}
face global StatusLineMode     ${rgb(colors.common.accent)}+b
face global StatusLineValue    ${rgb(colors.syntax.error)}`
  )
})
