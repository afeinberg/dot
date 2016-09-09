// Quick hack: convert e.g., CHAPTER1.PDF to Chapter1.pdf

package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"unicode"
)

func firstUpperLastLower(s string) string {
	out := []rune(strings.ToLower(s))
	out[0] = unicode.ToUpper(out[0])
	return string(out)
}

func traverseDir(path string) {
	files, _ := ioutil.ReadDir(path)
	for _, f := range files {
		if f.IsDir() {
			traverseDir(filepath.Join(path, f.Name()))
		}
		newname := firstUpperLastLower(f.Name())
		fullPathOld := filepath.Join(path, f.Name())
		fullPathNew := filepath.Join(path, newname)
		fmt.Printf("%q -> %q\n", fullPathOld, fullPathNew)
		if newname != f.Name() {
			os.Rename(fullPathOld, fullPathNew)
		}
	}
}

func main() {
	traverseDir(".")
}
