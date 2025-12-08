EMOJI_NEUTRAL=(😃 🤓 👾 🤖 💯 🐵 🦍 🐺 🐈 🦄 🦅 🦉 🐬 🐋 🐙 🌲 🌳 🍀 🍁 🍇 🍍 🍩 🌍 🌎 🌏 🚄 🚍 🚲 🛴 🚡 🚠 🚀)
EMOJI_UNHAPPY=(😵 😲 🤡 👹 😿 💔 💢)
EMOJI_PROMPT="${EMOJI_NEUTRAL[$RANDOM % ${#EMOJI_NEUTRAL[@]}]}"
EMOJI_PROMPT_ERROR="${EMOJI_UNHAPPY[$RANDOM % ${#EMOJI_UNHAPPY[@]}]}"

PROMPT="%(?.$EMOJI_PROMPT .$EMOJI_PROMPT_ERROR [%F{red}%?%f] )%B%F{blue}%c%f%b %F{blue}»%f "
RPROMPT=
