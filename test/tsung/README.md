To run tests prepare random files using auxiliary script *gen_files.pl* (you may
need to be a superuser).

```ShellSession
# mkdir -p /usr/local/webdata/static     # if needed
# touch /usr/local/webdata/static/content-empty.txt
# echo -n A > /usr/local/webdata/static/content-1byte.txt
# ./gen_files.pl -n1 -s50; mv test1.data /usr/local/webdata/static/content-small.txt
# ./gen_files.pl -n1 -s100k; mv test1.data /usr/local/webdata/static/content-large.txt
# ./gen_files.pl -n1 -s1M; mv test1.data /usr/local/webdata/static/content-huge.txt
```

Put actual *md5sum* values of generated files into *static.xml* and run nginx
and tsung.

```ShellSession
# nginx -c`pwd`/nginx.conf
# tsung -f static.xml start
```

