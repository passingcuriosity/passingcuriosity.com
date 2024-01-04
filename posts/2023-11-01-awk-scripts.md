---
title: Notes on AWK scripting
tags: howto, awk, scripting
excerpt: |
  A few notes about using AWK to write more substantial scripts.
---

# Writing AWK _scripts_

Many large AWK scripts are wrapped in a useless
shell script. If all of your logic is in AWK
then there's no need for the shell to get
involves in things; just use the correct
shebang!

```
#!/usr/bin/awk -f

BEGIN { print("start") }
END { print("end") }
```

Just make it executable and now you have a
script written directly in AWK with no
shells involved. Just make it executable
(with `chmod +x` as usual) and you are
good to go.

Why bother? It makes the script simpler
(e.g. no multiple levels of quoting and
escaping), makes it slightly less resource
intensive to start (only a single fork and
exec) and run (no shell waiting around for
the `awk` interpreter to finish), and makes
the script sightly easier to handle with
tools like syntax highlighting, code
formatting, etc.

# Code formatting

Like any body of code, the formatting of a
longer AWK script can be an important help
or hindrance to anyone trying to understand
it. GNU AWK has a helpful option to format 
AWK scripts.

```
#!/usr/bin/awk -f
BEGIN { print("start"); }
END   { print("end") }
```

We can format this AWK script like so:

```
$ gawk -f example.awk -o-
#!/usr/bin/awk -f
BEGIN {
        print("start")
}

END {
        print("end")
}
```

We use `-f` to read the AWK script from a
file and `-o` to format and output the
script.

In this case, we're writing it to the
standard output (`-o-`) but we could also
write it to another (different!) file with
`-oformatted.awk`) or use `-o` and let
`awk` write it to the default output file
(`awkprof.out`).
