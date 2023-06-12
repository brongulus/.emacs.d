;;;; -*-lisp-*-

;;;; This is a starter config file for viper.  Name it .viper and put
;;;; it in your home directory before you load viper up.  This file is
;;;; intended to make Viper sane for that tiny fraction of the
;;;; population who first learned emacs, and are using viper to learn
;;;; vi, instead of the other way around.  Any questions or comments
;;;; may be directed to hawkins@stanfordalumni.org
;;;; LAST UPDATED: Apr. 8, '01

;;;;;;;;;;; Code follows this line

;;;; I don't actually use this line, because I like the ex behavior.
;;;; But if you keep getting annoyed that deletion stops at the
;;;; beginning of the line, this line fixes that.
(setq viper-ex-style-editing nil)

;;;; This line does the same thing for the motion keys.  You probably
;;;; want both or neither...
(setq viper-ex-style-motion nil)

;;;; This isn't really emacs specific, it just gets rid of the start
;;;; up message, which you don't need if you've already found this
;;;; file.  :)  
(setq viper-inhibit-startup-message 't)

;;;; This refers to emacs expert level...so if you are coming from
;;;; emacs to begin with, you want max expert level set.
(setq viper-expert-level '1)

;;;; This causes viper to ignore case when doing searches (such as
;;;; with the '/' key.  This is more in keeping with 'C-s' searches,
;;;; so I prefer it.  You are welcome to unset this.  it's a fairly
;;;; minor point
(setq viper-case-fold-search 't)

;;;; If you don't have this line, C-d will not delete in insert state,
;;;; which can be confusing...  The default binding is to back tab.
;;;; If you really like the back tab function, either re-bind it or
;;;; give up the C-d deletion in insert state...arguably you should be
;;;; using 'x' in vi state to do deletion anyway.  Personally I delete
;;;; every third character I type, reflexively, and so this is
;;;; non-optional.
(define-key viper-insert-global-user-map "\C-d" 'delete-char)

;;;; These three lines ensure that C-v and M-v both work.  If you do
;;;; not set these, M-v will work (keeping in mind the possible
;;;; re-mapping of the meta key for viper), but C-v will do other
;;;; things depending on the state.  Note that this removes the viper
;;;; binding for C-v in vi state, which opens a file in a new emacs
;;;; session, but I've never seen that as much of a loss.  The
;;;; relevant function is called 'find-file-other-frame', I believe,
;;;; so you can re-bind it to something if you care to.  Also this
;;;; removes the binding of C-v to 'quoted insert, but this is also
;;;; bound to C-q, so that's not a big issue.
(define-key viper-vi-global-user-map "\C-v" 'scroll-up)
(define-key viper-insert-global-user-map "\C-v" 'scroll-up)
;(define-key viper-insert-global-user-map "\C-c\C-v" 'quoted-insert)

;;;; Viper maps C-w in insert state to delete back one word.  If you
;;;; are coming from emacs, you'll be expecting this to kill the
;;;; region into the ring.  The next line adjusts that. 
(define-key viper-insert-global-user-map "\C-w" 'kill-region)

;;;; These two give you back the ability to yank text in the vi state.
;;;; The default binding is to scroll the screen by a line.  I've
;;;; shown how to move this C-S-e, but I think that binding is
;;;; meaningless.  If you actually want 'viper-scroll-down-one' bound
;;;; to something, you'll probably need to pick something else.  I
;;;; bind C-e to 'nil in vi state, so that I don't confuse myself when
;;;; I reflexively try to go to the end of the line.  This is a useful
;;;; strategy to mask out keys which you expect to do something other
;;;; than they do, and want to unlearn.  'viper-nil doesn't do
;;;; anything harmful like jump the cursor around or alter text.
(define-key viper-vi-global-user-map "\C-y" 'yank)
(define-key viper-vi-global-user-map "\C-e" 'viper-nil)
;(define-key viper-vi-global-user-map "\C-\S-e" 'viper-scroll-down-one)

;;;; This next is probably the most crucial re-binding in this file.
;;;; If you don't set this line, C-h moves backwards a character.  For
;;;; those of us used to relying on it for help, this is kind of
;;;; scary.  :)
(setq viper-want-ctl-h-help t)


;;;;;;;;;;;;;; The remainder of this file is not necessary, it is just
;;;;;;;;;;;;;; things I think are worth adding to your file.  If any
;;;;;;;;;;;;;; of these things confuse you terribly, consider
;;;;;;;;;;;;;; commenting them out.


;;;; These two bindings don't overwrite anything, and they allow you
;;;; to cycle the binding of the '.' key.  Try it.  It's really handy.
(define-key viper-vi-global-user-map "\M-p" 'viper-prev-destructive-command)
(define-key viper-vi-global-user-map "\M-n" 'viper-next-destructive-command)

;;;; The default behavior of 'v' and 'V' confuses me, so I reverse them.
(setq ex-cycle-other-window nil)

;;;; These two are nice if you are using a real xemacs session.  If
;;;; you're in a tty, they have no effect.  You don't have to stick to
;;;; my color choices.  :)
(setq viper-insert-state-cursor-color "blue")
(setq viper-replace-overlay-cursor-color "black")

;;;; Finally, this just enables the 'g' key in vi state.  Read about
;;;; it in the manual.  I can't think why you wouldn't want it active.
(viper-buffer-search-enable)

;;;; A few notes, finally, on transitioning to viper if you've never
;;;; used vi key bindings or functionality before:

;;;; The oddest thing to get used to is the vi motion commands.  Using
;;;; h,j,k and l to move is very unnatural if you've never done it
;;;; before.  Personally, I'm a nethack addict from way back, so this
;;;; wasn't a problem for me.  If you want to learn these keys, I
;;;; recommend picking up a copy of the game, and playing with the
;;;; nonumber_pad option set until you're comfortable with the
;;;; motions.  Note, though, that the diagonal commands from nethack
;;;; are not recognized in vi.  (Moving diagonally is more or less
;;;; irrelevant in an editor, so those keys are bound to much more
;;;; useful things, like backing up a word at a time and copying text
;;;; and undoing destructive commands.)  You may think it's odd that I
;;;; recommend a separate program to learn how to move in viper, but
;;;; consider that a) trying to learn to move at the same time you try
;;;; to do useful work can be a pain in the whatzit, and b) nethack is
;;;; a great game once you learn the basics, so you ought to be
;;;; playing it anyway.  ;) It can be found at http://www.nethack.org
;;;; for every platform under the sun.

;;;; Another thing to note is that in vi the cursor is always on a
;;;; character if it can be.  In particular this means that in vi
;;;; state you can't normally get the cursor past the last character
;;;; on a given line.  This is confusing, but it makes sense once you
;;;; get used to vi.  To add text to the end of a line, you don't have
;;;; to move there first.  Typing 'A' takes you into insert state and
;;;; goes to the end of the line all at once.  'I' does the same for
;;;; the start of the line.  Also note that when you back out of
;;;; insert mode, the cursor backs up.  You can 'fix' this with a
;;;; canned customization mentioned in the manual, but this is
;;;; probably not wise.  this idiosyncrasy will make more sense once
;;;; you've used viper for a while.  in the meantime, just get used to
;;;; it.

;;;; Learn the mnemonic of combining destructive commands with
;;;; movement commands.  Once you get the hang of this, it's really
;;;; efficient and versatile.  You'll quickly get annoyed with emacs'
;;;; requiring that you use a different key binding for every possible
;;;; combination, or just not allowing it.  :) Remember that the
;;;; motion command 'r' makes the command apply to the region.

;;;; Take your time learning viper.  Remember that (especially with a
;;;; .viper like this one) insert state is virtually indistinguishable
;;;; from normal emacs behavior.  If you are at a computer where Meta
;;;; and ESC are not distinct entities, you'll need to learn to use
;;;; C-\ for Meta, but if you are using a windowed version of xemacs
;;;; (i.e. not running inside a terminal client), this probably isn't
;;;; even an issue.  So spend a lot of time in insert state until you
;;;; feel comfortable moving around in vi.  Try to gradually wean
;;;; yourself from spending any more time than ABSOLUTELY necessary in
;;;; insert state.  If you need to add text, enter insert state
;;;; somehow, add the text, and then ESC out.  When you need to do
;;;; repeated edits, consider whether repeating inserts with '.' could
;;;; make things more efficient.  Always be looking for ways to
;;;; incorporate vi commands into your work.  Often, they are more
;;;; efficient.  It is amazing how much speed you can gain as a result
;;;; of having the entire qwerty keypad real estate devoted to
;;;; commands.  

;;;; One command combination I find really effective is using the
;;;; following process to do query-replacement of text: search for the
;;;; text with '/', type 'cw' or equivalent to select the text for
;;;; replacement, and then type the replacement text and ESC out.  Now
;;;; you can 'n' to find the next occurrence of the search text, and
;;;; hit . to replace the current instance.  Why, you ask, should you
;;;; bother with anything more complicated than 'M-%' or 'Q' (in vi
;;;; state)?  Well, basically because it makes search-replace
;;;; completely interruptible, and interactively modifiable.  For
;;;; example, you can easily switch the replacement text by hitting
;;;; 'cw' again, or the search text by hitting '/' again.  And you can
;;;; go do other editing of any form you want, and then resume the
;;;; search-replace with a single key ('n').  Very slick.  And you can
;;;; use the M-p and M-n commands to move between earlier search text
;;;; or replacement text.  I just think this puts sliced bread in the
;;;; poor house, myself.  :)

;;;; Have fun!  And if you have any comments, criticisms, or questions
;;;; about this file, you are encouraged to send them to John Hawkins
;;;; at hawkins@stanfordalumni.org.

;;;; This code and text is supported only in the sense that the author
;;;; will try to be supportive of comments, requests, and so on,
;;;; pertaining to it.  No warranty is expressed or implied, use at
;;;; your own risk, etc.
