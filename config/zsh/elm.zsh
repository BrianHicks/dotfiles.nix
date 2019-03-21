## work with Elm files

# look at the outline of an Elm file. Useful for seeing if an API is self-describing.
elm_outline() {
    MODULE=$1
    grep -E '^(type alias [\w ]+|type [\w ]+|\w+ : .+)' $MODULE
}
