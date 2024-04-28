#### Load tests with tsung

##### Prepare static files

Prepare random files using auxiliary script *gen_files.pl*. The files will be
embedded in the Haskell source code on the next step.

```ShellSession
$ STATIC_DIR=/var/lib/nginx/test/tsung/static
$ sudo install -d $STATIC_DIR
$ sudo touch $STATIC_DIR/content-empty.txt
$ sudo sh -c "echo -n A > $STATIC_DIR/content-1byte.txt"
$ ./gen_files.pl -n1 -s50; sudo mv test1.data $STATIC_DIR/content-small.txt
$ ./gen_files.pl -n1 -s100k; sudo mv test1.data $STATIC_DIR/content-large.txt
$ ./gen_files.pl -n1 -s1M; sudo mv test1.data $STATIC_DIR/content-huge.txt
```

##### Build and install Haskell library

```ShellSession
$ make
$ sudo make install
```

##### Static content handlers

Put actual *md5sum* values of generated files into *static.xml*,

```ShellSession
$ for f in $STATIC_DIR/content-* ; do
>     NAME=$(basename $f) ; MD5HEX=$(md5sum $f | cut -d' ' -f 1) ;
>     sed -r -i "/md5hex/N; s/(md5hex'>).*(<\\/match>.*$NAME.*\$)/\\1$MD5HEX\\2/" static.xml
> done
```

and run nginx and tsung.

```ShellSession
$ nginx -c`pwd`/nginx-static.conf
$ tsung -f static.xml start
```

##### Asynchronous tasks

Run

```ShellSession
$ nginx -c`pwd`/nginx-async.conf
$ tsung -f async.xml start
```

