if test -d "/opt/homebrew/opt/llvm@13"; then
    export PATH="/opt/homebrew/opt/llvm@13/bin:$PATH"
    export LDFLAGS="-L/opt/homebrew/opt/llvm@13/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/llvm@13/include"
fi
