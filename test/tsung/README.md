#### Static content handlers

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

Put actual *md5sum* values of generated files into *static.xml*,

```ShellSession
# for f in /usr/local/webdata/static/content-* ; do
>     NAME=$(basename $f) ; MD5HEX=$(md5sum $f | cut -d' ' -f 1) ;
>     sed -r -i "/md5hex/N; s/(md5hex'>).*(<\\/match>.*$NAME.*\$)/\\1$MD5HEX\\2/" static.xml
> done
```

and run nginx and tsung.

```ShellSession
# nginx -c`pwd`/nginx-static.conf
# tsung -f static.xml start
```

#### Asynchronous tasks

Run

```ShellSession
# nginx -c`pwd`/nginx-async.conf
# tsung -f async.xml start
```

