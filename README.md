# Jade

Jade is a framework for creating text-based multiplayer role-playing games. You
could say it is an updated take on a MUD. Jade is designed to be played in a
modern browser and includes an integrated HTML5 client.

## Installation (or, Getting Started with Common Lisp)

Jade is written in [Common Lisp](https://common-lisp.net), which is perhaps not
the most widely-used language these days. As such, this section details the
process of setting up a functional Common Lisp development environment in
addition to describing the specific requirements for running Jade itself.

For a more general introduction, see e.g.
[lisp-lang.org](https://lisp-lang.org/learn/getting-started/).

The steps below are specific to macOS. They will differ for other OSes but the
general outline is the same.

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

The following steps are specific to dependencies required by Jade:

* Install [NGINX](https://www.nginx.com/): `brew install nginx`.

* Install [SQLite](https://www.sqlite.org/): `brew install sqlite3`.

* Install [libuv](https://libuv.org): `brew install libuv`.

+ Install [Pandoc](https://pandoc.org): `brew install pandoc`. Pandoc is used to
  build client web pages and documentation from markdown.

* So that Quicklisp can find the Jade project, create a symlink inside the
  `$HOME/quicklisp/local-projects` directory that points to the directory
  containing this README file. For example:

````
cd ~/quicklisp/local-projects
ln -s ~/Projects/jade
````

* Load the Jade project, including its dependencies. From within the REPL,
  type `(ql:quickload "jade")`.

That's a lot of steps! But you only have to do them once.

## Usage

In this section, `$JADE` is assumed to refer to the root directory on your
machine of the Jade source code, i.e. the directory that contains "jade.asd".

### Create the Data Directory

Jade uses `/usr/local/var/jade` as a root directory for data used by the
running server, and as a place to put server logs. Start by creating this
directory and linking it to the client resources:

```
mkdir /usr/local/var/jade
cd /usr/local/var/jade
ln -s $JADE/genfiles/client/
ln -s $JADE/data/
```

### Start NGINX

Jade uses NGINX as a webserver that sits in front of the game server to serve
static content and provide TLS functionality.

After installing NGINX, you can start it as follows:

```
sudo nginx -c $JADE/config/nginx.conf
```

By default, NGINX listens on ports 80/443. If you cannot use the standard ports
you will need to modify the configuration file.

### Create the Database

Before running the server, create the database used to store account
information, player avatars, etc.

```
sqlite3 /usr/local/var/jade/jade.db < $JADE/config/schema.sql
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

### Connect as a Client

Connect to `http://localhost/`. Follow the instructions to create a new account
and you'll be dropped into the game.
