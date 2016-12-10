---
title:     "X sessions with s6"
author:    "ertes"
date:      "2016-12-10"
lang:      "en"
copyright: "Ertugrul Söylemez"
want-toc:  "yes"
...

A typical X setup for many people is to have an `.xsession` script or
something similar to start their window manager along with any number of
background applications that are relevant to the graphical session.
However, this always felt fragile to me and on some occasions actually
broke, confirming my feeling.  So a few months ago I was looking for a
more principled and robust approach, and I figured: hell, why not use an
actual process supervisor?

The idea is simple: Most supervisors don't actually insist on running as
PID 1 (the system's init process) or even root for that matter, even
though that's their most prominent use case.  They will manage your
processes just fine without any of that.  Since I have successfully used
the [s6 supervision suite][s6] in the context of Linux containers, I
thought it could just as well manage my X session, because I actually
have quite a few background processes running all the time, including
*compton*, *MPD*, *redshift*, *unclutter*, *urxvtd* and *xscreensaver*.

[s6]: http://skarnet.org/software/s6/


Supervised processes
====================

A supervised process is just a process that runs under the wing of s6.
That means that s6 will take care of starting and terminating it, and it
will be aware of its current state at all times.  Also it will make sure
that, if enabled, the process is actually running, and that *exactly
one* instance of it is running.  For example when the process crashes s6
will restart it.  Moreover it will be the correspondent in communicating
(or in most cases *not* communicating) with the process through stdin,
stdout and stderr.  Most notably it will handle logging, if you ask for
it.

S6 will supervise any process that doesn't fork.  Many programs are
written in a less unixy way in that they fork by default and log to
syslog, but most of them have a "foreground" mode, which does exactly
the right thing: do not fork, and log to stderr instead of syslog.
There is one notable exception, the Emacs server, which I will discuss
below.

The first step to using s6 is to turn your processes into supervised
processes.  Create a directory somewhere, called your *scan directory*,
which will hold a subdirectory for each background process.  We will set
up *MPD* and *urxvtd* as examples, starting with the latter, but the
same steps will work for any other process.  I will assume that your
scan directory is named `~/run`, and all relative paths in this article
will be relative to that directory (adjust the commands below, if you
name it differently):

    mkdir ~/run
    cd ~/run

Note that this directory needs to be writable during your session, so
don't put it on a read-only filesystem.

You're probably tempted to call these supervised processes *services*,
but don't do that.  Services are more than just processes, and there is
a separate service management suite built on top of s6, which is not the
topic of this article.


Example: urxvtd
---------------

My terminal emulator is called [rxvt-unicode][].  Typically and
preferably every terminal is its own processes, but if you use lots of
terminals and create/discard them on the fly all the time (as I do),
there is some speed and memory benefit in using *urxvtd*, the
rxvt-unicode *daemon*.

Its default mode of operation is as a regular foreground process, which
makes it really easy to supervise.  First create a directory for it,
called its *service directory* (yes, the term is inappropriate given my
note about "services" above, but that's s6 terminology, so we'll go with
it):

    mkdir urxvtd

(One-time reminder: all paths are relative to `~/run`.)  Now create a
file named `urxvtd/run` containing the following shell script:

    #! /usr/bin/env sh
    cd
    exec urxvtd

Now make that file executable:

    chmod 755 urxvtd/run

That's it.  The `run` script will be used by s6 as the process to
supervise.  Let me repeat that: from the viewpoint of s6 for all intents
and purposes this script *is* the program it should supervise, which is
the reason why we *exec* into the daemon at the end after some minor
setup.  The `cd` command just switches into your home directory before
executing urxvtd, because s6 starts processes in the service directory,
and you probably don't want all your shells to start there.

[rxvt-unicode]: http://software.schmorp.de/pkg/rxvt-unicode.html


First test run
--------------

Now let's fire up this baby and see how well it works.  To invoke s6 on
a scan directory start the *scanner* called *s6-svscan* on it:

    s6-svscan .

You will immediately notice that this too is a foreground process, so
you need an extra shell to continue from here (resist the temptation to
send s6-svscan to the background for now).  Remember to `cd ~/run` in
the new shell.  The second thing you will notice is that you get a bunch
of new running processes as a side effect, and among them you will
indeed find urxvtd.  Great!

The other two processes are s6-svscan, the one you just started, and
*s6-supervise*, the actual supervisor for individual processes.  Don't
worry about the number of processes, as the s6 processes are designed to
be extremely small.  They do their job and nothing else.

You can control the supervisors of individual processes through
`s6-svc`.  For example you can temporarily disable urxvtd by setting its
state to *down*:

    s6-svc -d urxvtd

The argument `urxvtd` is actually the path to the service directory.  If
your current working directory weren't `~/run`, you would have to write
something like:

    s6-svc -d ~/run/urxvtd

This command communicates with s6-supervise and in this case requests to
terminate the process (and not restart it).  S6-supervise will
immediately kill the process, and you will find that urxvtd is no longer
running.  You can bring it back up by setting its state to *up* again:

    s6-svc -u urxvtd

Note that s6 always starts from a clean state.  That means that the
process state is not preserved across runs of s6-svscan.  There is a way
to disable a process permanently without deleting it, but more on that
later.  Another interesting thing to note is that if you kill the urxvtd
process by yourself using e.g. `kill`, or if it crashes for some reason,
its state will still be *up*.  S6-supervise notices when the process
dies unexpectedly and restarts it after a delay of one second.

There is a separate command called *s6-svscanctl* to control the scanner
itself, the root of your process tree.  For the most part you will only
need this command in two situations: either to stop the scanner, which
will amount to terminating your X session, or to *rescan* the scan
directory for new processes.  Run the following command now to terminate
the scanner:

    s6-svscanctl -q .

This command communicates with the s6-svscan instance running in the
given directory and tells it to exit, and you can watch your s6-svscan
finish and return to your shell.  However, it will also print a bunch of
worrying messages before it exits:

    s6-svscan: warning: unable to exec finish script .s6-svscan/finish: No such file or directory
    s6-svscan: warning: executing into .s6-svscan/crash
    s6-svscan: fatal: unable to exec .s6-svscan/crash: No such file or directory

The problem here is that s6-svscan normally refuses to exit by itself.
Remember that it was designed as a process that runs as PID 1,
a.k.a. the init process, which must never exit.  An exiting init process
causes a kernel panic.  Therefore the normal way to shut it down is to
exec into something else, for example a script that handles rebooting
the system or powering it off.

However, we're not using it as an init process, so we actually want it
to exit.  The easiest way to do that is to make it exec into something
trivial that does nothing, for example an empty shell script.  The first
warning indicates that s6-svscan execs into `.s6-svscan/finish` when it
is told to exit.  The `.s6-svscan` directory should already exist now,
because it is created by s6-svscan.  Run the following commands:

    echo '#! /usr/bin/env sh' > .s6-svscan/finish
    chmod 755 .s6-svscan/finish

If you want you can put some gimmicks into that script to make it print
something at the end of your session.  We will do that just to see that
it works:

    echo 'echo "Good bye."' >> .s6-svscan/finish

Now start s6-svscan again:

    s6-svscan .

And from the other shell tell it to exit again:

    s6-svscanctl -q .

And indeed instead of warnings it exits cleanly with the message "Good
bye." that we put into our `finish` script.  Alright, let's start it
again:

    s6-svscan .

Just leave it running from here.


Example: MPD
------------

With the [Music Player Daemon][mpd] (MPD), it's pretty much the same
story, but with a catch: MPD operates in daemon mode by default, which
is undesirable.  Fortunately it does offer a foreground mode that you
can enable through the `--no-daemon` command line option.  Again create
a service directory for it:

    mkdir mpd

Create the script `mpd/run` as you did with urxvtd before, but pass the
`--no-daemon` option to MPD, and also don't make it executable just yet:

    #! /usr/bin/env sh

    cd
    exec mpd --no-daemon

If your version of s6 is older than 2.3, then you will notice that as
soon as you have created the directory, s6-svscan will periodically
produce warnings that it can't execute your `run` script.  This is
because it used to rescan the scan directory periodically by default,
but since version 2.3 it no longer does.  Using s6-svscanctl you can
tell it to rescan when you add new supervised processes (`-a`) or to
kill supervised processes of service directories that you have deleted
(`-n`):

    s6-svscanctl -an .

At least since running that command you should see the warnings now:

    s6-supervise mpd: warning: unable to spawn ./run - waiting 10 seconds
    s6-supervise (child): fatal: unable to exec run: Permission denied

They will be printed periodically until you make the script executable:

    chmod 755 mpd/run

After that command the supervisor will (hopefully) succeed to start MPD.
And as with urxvtd you can manage the process through s6-svc.

Ok, we're done with the supervisor tutorial.  In the next major section
we will learn how to add your window manager to the mix and manage your
entire X session using s6.  For now just shut down s6-svscan again:

    s6-svscanctl -q .

[mpd]: https://www.musicpd.org/


Disabling processes
-------------------

As explained earlier all state changes made through `s6-svc` are
ephemeral.  If you want to disable a process permanently without
deleting its service directory, all you need to do is to create the file
`down` there.  For example to disable MPD, you would use the sporty
command sequence:

    touch mpd/down
    s6-svc -d mpd

The second command is needed, because the supervisor does not shut it
down automatically, if it's currently running.  It only uses the
existence of the `down` file to decide the initial state of the process.
In order to enable it again, just delete that file:

    rm mpd/down
    s6-svc -u mpd


Emacs server
------------

Unfortunately some programs resist being supervised that easily.  The
(GNU) Emacs server is a well known example of that, because it insists
on forking, if you use the `--daemon` command line option.  I don't have
a good answer on how to supervise those.

One solution I have found online is not to use `--daemon` at all, but
start a regular Emacs instance in a Screen session, then invoke the
Elisp function `(server-start)`.  Screen is a foreground process that
doesn't require a terminal on stdin/stdout, so it can be supervised
easily.

The real solution is to fix those daemons.


The window manager
==================

Now that you know how to use s6 as your process supervisor, you can go
one step further and make s6-svscan the main process of your graphical
session.  It can also take care of starting your window manager.
Therefore your window manager no longer has a special role.  Indeed you
can restart it or even switch to a different one without closing your
session.

The first step is to create another supervised process for your window
manager.  Most window managers are designed to be started from X session
scripts, so they are regular foreground processes.  Thus the setup
process is the same as for urxvtd above.

After that adjust your session script (most likely `~/.xsession`) to
exec into s6-svscan after running all one-shot commands.  Example of
what a typical session script might look like:

    #! /usr/bin/env sh

    xmodmap ~/cfg/xmodmap
    xrdb -merge ~/cfg/xresources
    xset r rate 250 30
    xsetroot -cursor_name left_ptr
    feh --bg-fill ~/gfx/wallpaper.jpg

    exec s6-svscan ~/run

Remember that you no longer start background daemons from here.  Turn
those into supervised processes instead.

Once you start your X session this way you can no longer terminate it by
telling your window manager to quit, because s6-supervise would simply
restart it and nothing else would happen.  Instead you may want to bind
a keyboard shortcut to the following command:

    s6-svscanctl -q ~/run


Conclusion
==========

S6 should make your X sessions a lot more robust and potentially also
easier to manage.  No longer will you need PID files, and s6 has many
features that I didn't discuss here, so this article merely scratches
the surface.  Of course most of these features (logging, readiness
notifications, socket activation, etc.) are mainly useful for system
services rather than background programs of your graphical session, but
perhaps you find some use for them.

Also as noted earlier s6 only does process supervision.  There are at
least two service managers built on top of s6 with the most obvious one
being [s6-rc][] that is written by the same people, and a third-party
one called [anopa][], each with different trade-offs.  These are useful,
if you have dependencies between the processes, or if you have a bunch
of one-shot services (services that don't correspond to a running
daemon).  For example if you attach a beamer to your laptop, you could
have a *beamer* service that runs the appropriate `xrandr` commands to
enable the HDMI/VGA output when started and disable it when stopped.
You can go as far as to trigger the service in response to connecting a
display device to a certain port, but this is far beyond the scope of
this article.

Have fun!

[anopa]: http://jjacky.com/anopa/
[s6-rc]: http://skarnet.org/software/s6-rc/

