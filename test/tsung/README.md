To run tests prepare random files using auxiliary script *gen_files.pl*.

```ShellSession
# mkdir -p /usr/local/webdata/static     # if needed
# touch /usr/local/webdata/static/content-empty.txt
# echo -n A > /usr/local/webdata/static/content-1byte.txt
# ./gen_files.pl -n1 -s50; mv test1.data /usr/local/webdata/static/content-small.txt
# ./gen_files.pl -n1 -s100k; mv test1.data /usr/local/webdata/static/content-large.txt
# ./gen_files.pl -n1 -s1M; mv test1.data /usr/local/webdata/static/content-huge.txt
```

Put actual *md5sum* values of generated files into *static.xml* and run tsung.

```ShellSession
# tsung -f static.xml start
```

