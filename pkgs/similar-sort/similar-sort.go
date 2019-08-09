package main

import "bufio"
import "fmt"
import "os"
import "sort"
import "strings"
import "unicode/utf8"

func main() {
	target := strings.Join(os.Args[1:], " ")

	s := bufio.NewScanner(os.Stdin)
	var lines []WithDistance
	for s.Scan() {
		lines = append(lines, WithDistance{s.Text(), Levenshtein(target, s.Text())})
	}

	sort.Slice(lines, func(i, j int) bool {
		return lines[i].distance < lines[j].distance
	})

	for _, line := range lines {
		fmt.Println(line.text)
	}
}

type WithDistance struct {
	text     string
	distance int
}

// https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Go
func Levenshtein(a, b string) int {
	f := make([]int, utf8.RuneCountInString(b)+1)

	for j := range f {
		f[j] = j
	}

	for _, ca := range a {
		j := 1
		fj1 := f[0] // fj1 is the value of f[j - 1] in last iteration
		f[0]++
		for _, cb := range b {
			mn := min(f[j]+1, f[j-1]+1) // delete & insert
			if cb != ca {
				mn = min(mn, fj1+1) // change
			} else {
				mn = min(mn, fj1) // matched
			}

			fj1, f[j] = f[j], mn // save f[j] to fj1(j is about to increase), update f[j] to mn
			j++
		}
	}

	return f[len(f)-1]
}

func min(a, b int) int {
	if a <= b {
		return a
	} else {
		return b
	}
}
