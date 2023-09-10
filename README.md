# Jade

Jade is a framework for creating text-based multiplayer role-playing games. You
could say it is an updated take on a MUD. Jade is designed to be played in a
modern browser and includes an integrated HTML5 client.

## Getting Started with Common Lisp

Jade is written in [Common Lisp](https://common-lisp.net), which is perhaps not
the most widely-used language these days. As such, this section details the
process of setting up a functional Common Lisp development environment. Skip to
the next section of you're already a Lisp hacker!

The steps outlined below are specific to macOS. For a more general introduction
to setting things up, see
[lisp-lang.org](https://lisp-lang.org/learn/getting-started/).

* Install [homebrew](https://brew.sh).

* Install [SBCL](https://sbcl.org): `brew install sbcl`.

* Install [Emacs](https://www.gnu.org/software/emacs/): `brew install emacs`.

* Install [Slime](https://common-lisp.net/project/slime/), which is a Common
  Lisp development environment for Emacs. You should be able to install it from
  within Emacs with `M-x package-install RET slime RET`.

* Configure Slime by adding the following to your Emacs init file:

```
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime-autoloads)
(setq slime-contribs '(slime-asdf slime-repl slime-fancy))
```

* You can now fire up SBCL within Emacs using `M-x slime`. This will dump you
  into the SBCL/Slime REPL where you can evaluate Lisp forms.

* Install [Quicklisp](https://www.quicklisp.org/), which is a library
  manager for Common Lisp. Download the `quicklisp.lisp` file and then, from
  within the REPL, type:

```
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:add-to-init-file)
```

By the way, if you're interested in learning the Lisp language I can highly
recommend the book [Practical Common Lisp](https://gigamonkeys.com/book/).

## Installing Dependencies

The following steps install dependencies required by Jade:

* Install [NGINX](https://www.nginx.com/): `brew install nginx`.

* Install [SQLite](https://www.sqlite.org/): `brew install sqlite3`.

* Install [libuv](https://libuv.org): `brew install libuv`.

+ Install [Pandoc](https://pandoc.org): `brew install pandoc`. Pandoc is used to
  build client web pages and documentation from markdown.

## Seting Up Quicklisp

* So that Quicklisp can find the Jade project, create a symlink inside the
  `$HOME/quicklisp/local-projects` directory that points to the directory
  containing this README file. For example:

````
cd ~/quicklisp/local-projects
ln -s ~/Projects/jade
````

* Load the Jade project, including its dependencies. From within the REPL,
  type `(ql:quickload "jade")`.

## Running Jade

In this section, `$JADE` is assumed to refer to the root directory on your
machine of the Jade source code, i.e. the directory that contains "jade.asd".

### Build the Client Data

In the `$JADE` directory, type `make`. This will create a `build` directory,
which itself contains a few files and subdirectories. The `client` subdirectory
contains everything that will be served to the user's browser via nginx. The
`data` subdirectory contains files consumed by the game server.

### Link to the Client Directory

The file `$JADE/src/config.lisp` configures the game server. One of the settings
is `root-directory`, which is the path of the directory containing client files.
During development, you can change this setting to refer to your `build`
directory.

### Start NGINX

Jade uses NGINX as a webserver that sits in front of the game server to serve
static content and provide TLS functionality.

After installing NGINX and building Jade, you can start it as follows (from
within the top-level Jade directory):

```
nginx -p `pwd`/build -c nginx.conf
```

### Create the Database

Before running the server, create the database used to store account
information, player avatars, etc.

```
sqlite3 $JADE/build/jade.db < $JADE/build/schema.sql
```

### Run the Server

During development, the most convenient way to run the server is within the
Slime REPL:

```
M-x slime ; starts the REPL
,load-system jade ; loads jade.asd
,in-package jade ; changes to the jade package
(run-server) ; starts the game server
```

### Connect to the Game

Connect to `http://localhost:8080/` (unless you've change the port in
`config.lisp`). Follow the instructions to create a new account and you'll be
dropped into the game.

Enjoy!
