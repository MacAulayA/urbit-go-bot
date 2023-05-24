/-  spider, *gato, urbit-go
/+  *strandio
=,  strand=strand:spider
|%
  ::  Parse the user input
  ::
  ::    A needle such as "foo=" in a haystack such as "this is foo=bar really"
  ::    Will return the value "bar"
  ::
  ++  parse-input
    |=  [nedl=tape hstk=tape default=tape]
    ^-  tape
    ?~  (find nedl hstk)  default
      =/  idx  (add (dec (lent nedl)) (need (find nedl hstk)))
      =/  rem  (slag +(idx) hstk)
      =/  extract  ?~  (find " " rem)  `tape`rem  `tape`(scag (need (find " " rem)) rem)
      extract
  ::
  ::  Write our own poke so that we can
  ::  get data send with a %poke-ack/%nack ????
  ::  (based on strandio.hoon raw-poke)
  ::
  ++  custom-poke
    |=  [=dock =cage]
    =/  m  (strand ,vase)
    ^-  form:m
    =/  =card:agent:gall  [%pass /poke %agent dock %poke cage]
    ;<  ~  bind:m  (send-raw-card card)
    =/  m  (strand ,vase)
    ^-  form:m
    |=  tin=strand-input:strand
    ?+  in.tin  `[%skip ~]
        ~
      `[%wait ~]
    ::
        [~ %agent * %poke-ack *]
      ?.  =(/poke wire.u.in.tin)
        `[%skip ~]
      `[%done !>(tin)]
    ==
  ::
--
=/  m  (strand ,vase)
^-  thread:spider
|=  arg=vase
^-  form:m
=/  =bird  !<(bird arg)

::
:: Parse text.bird
::

=/  msg-origin=@p  author.memo.bird
;<  our-ship=@p   bind:m  get-our

?.  =(msg-origin our-ship)
  :: eject Mailman, eject!!  (this message comes from someone else)
  ~&  "Message origin not our ship - ignoring"
  !!
::  Need to ensure we're only interacting with our own ship's urbit-go
=/  text-tape  (trip text.bird)
~&  "Message origin is our ship - running thread..."
::  Extract game action from input text.
=/  action-tape  (parse-input "%" text-tape "error")
=/  game-action  `@tas`(slav %tas (crip action-tape))

?+      game-action  (pure:m !>(['Go-chat: invalid action (try /ugo %help)' vase.bird]))
  :: Challenge someone to a game
  ::
  ::   [%challenge name=@t who=ship komi=@rs handicap=@ud size=@ud order=@tas{%random, %challenger, %challenged}]
  ::   Parsing inputs, with defaults for name, komi, handicap, size and order of play
  ::
    %challenge
  =/  ch  (parse-input "~" text-tape "error")
  =/  challenged  `@p`(slav %p (crip (weld "~" ch)))
  
  =/  name  (crip (parse-input "name=" text-tape "Unnamed game"))
  
  =/  km  (parse-input "komi=" text-tape ".6.5")
  =/  komi  `@rs`(slav %rs (crip km))
  
  =/  hc  `@ud`(slav %ud (crip (parse-input "handicap=" text-tape "0")))           
  =/  handicap  ?:(|((gth hc 9) (lth hc 0)) 0 hc)          :: Values between 0 and 9
  
  =/  sz  `@ud`(slav %ud (crip (parse-input "size=" text-tape "19")))                  
  =/  size  ?:(|(=(hc 19) =(hc 13) =(hc 9)) sz 19)       :: Sizes are 9x9, 13x13 and 19x19
  
  =/  od  `@tas`(crip (parse-input "order=" text-tape "random"))            
  =/  order  ?:(|(=(od %random) =(od %challenger) =(od %challenged)) od %random)  :: Values are %random, %challenger, %challenged

  ;<  our=@p   bind:m  get-our
  ;<  ~        bind:m  (poke [our %urbit-go] [%urbit-go-action !>([%challenge name=name who=challenged komi=komi handicap=handicap size=size order=order])])  ::check this poke structure
  (pure:m !>([(crip ;:(weld "Go: " (scow %p challenged) " you have been challenged by " (scow %p our) "!")) vase.bird]))
::
::  Accept game invitation
    %accept
  =/  challenger-a  `@p`(slav %p (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  challenge=(list go-challenge:urbit-go)  bind:m  (scry [(list go-challenge:urbit-go)] `path`['gx' 'urbit-go' 'challenges' 'noun' ~])
  =/  challenge-0  (snag 0 challenge)  :: first challenge in list
  =/  game-id=@dau  game-id.challenge-0
  ;<  ~        bind:m  (poke [our %urbit-go] [%urbit-go-action !>([%accept-challenge who=challenger-a])])
  (pure:m !>([(crip ;:(weld "Go: game accepted. " (scow %p our) " plays " (scow %p challenger-a) " , with game-id: " (scow %da game-id))) vase.bird]))
::
::  Decline game invitation
    %decline
  =/  challenger-d  `@p`(slav %p (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  ~        bind:m  (poke [our %urbit-go] [%urbit-go-action !>([%decline-challenge who=challenger-d])])
  (pure:m !>(['Go: game declined' vase.bird]))
::
::  Withdraw game invitation
    %withdraw
  =/  challenged-d  `@p`(slav %p (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  ~        bind:m  (poke [our %urbit-go] [%urbit-go-action !>([%withdraw-challenge who=challenged-d])])
  (pure:m !>(['Go: challenge withdrawn' vase.bird]))
::
::  Make a move
    %move
  =/  game-id-m  `@dau`(slav %da (crip (weld "~" (parse-input "~" text-tape "error"))))

  :: get move position
  =/  posn-tape  (slag (need (find "[" text-tape)) text-tape)                                    :: position data "[x y]"
  =/  t  `tape`(flop (snip (flop (snip posn-tape))))                                             :: "[x y]" to "x y"
  :: "x y" to [x y]
  =/  posn  [`@ud`(slav %ud (crip (scag (need (find " " t)) t))) `@ud`(slav %ud (crip (slag (add 1 (need (find " " t))) t)))] 

::original without errors returned---
::  ;<  our=@p   bind:m  get-our
::  ;<  ~        bind:m  (poke [our %urbit-go] [%urbit-go-action !>([%move id=game-id-m position=posn])])
::  =/  move-msg  (crip ;:(weld "Go: " (scow %p our) " moved to " posn-tape))
::  (pure:m !>([`reply`[%story [[[%image (crip ;:(weld "/~/scry/urbit-go/game/" (scow %da game-id-m) "/" (scow %da now) ".svg")) 300 300 'go board'] ~] [[move-msg] ~]]] vase.bird]))
::-----------

  ;<  our=@p    bind:m  get-our
  ;<  now=@da   bind:m  get-time
  ;<  byk=beak  bind:m  get-beak

  ~&  "kicking off a separate thread to watch for our poke"
  ;<  ted=tid:spider    bind:m  (start-thread-with-args [byk %go-poke !>([%move id=game-id-m position=posn])])
  ~&  "ted:"
  ~&  ted  :: this works when the thread simply checks the time, but not with a poke.

::  Doesn't work, even if we don't do the poke, it simply never returns a result, still failing on the take-fact I assume.
::  ~&  "running await thread to run %move"
::  ;<    =thread-result  bind:m  (await-thread %go-poke !>([%move id=game-id-m position=posn]))
::  ~&  "thread-result {<thread-result>}"

::  ~&  "running poke..."
::  ;<  ~        bind:m  (poke [our %urbit-go] [%urbit-go-action !>([%move id=game-id-m position=posn])])
::    ~&  "have poked, running take-poke.."
::  ;<  =vase  bind:m  ((handle ,vase) (take-poke %urbit-go-action))
::  ~&  "take-poke has run."
::  ~&  "vase is: "
::  ~&  vase
  =/  move-msg  (crip ;:(weld "Go: " (scow %p our) " moved to " posn-tape))
  (pure:m !>([`reply`[%story [[[%image (crip ;:(weld "/~/scry/urbit-go/game/" (scow %da game-id-m) "/" (scow %da now) ".svg")) 300 300 'go board'] ~] [[move-msg] ~]]] vase.bird]))
::
::  Pass on your turn
    %pass
  =/  game-id-p  `@dau`(slav %da (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  ~        bind:m  (poke [our %urbit-go] [%urbit-go-action !>([%pass id=game-id-p])])
  (pure:m !>([(crip (weld (weld "Go: " (scow %p our)) " passed.")) vase.bird]))
::
::  Resign the game
    %resign
  =/  game-id-r  `@dau`(slav %da (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  ~        bind:m  (poke [our %urbit-go] [%urbit-go-action !>([%resign id=game-id-r])])
  (pure:m !>([(crip (weld (weld "Go: " (scow %p our)) " resigned.")) vase.bird]))
::
::  View the state of the board
    %look
  =/  game-id  `tape`(weld "~" (parse-input "~" text-tape "error"))

  :: Scry urbit-go directly, as I've added an svg return type
  ;<  now=@da   bind:m  get-time
  (pure:m !>([`reply`[%story [[[%image (crip ;:(weld "/~/scry/urbit-go/game/" game-id "/" (scow %da now) ".svg")) 300 300 'go board'] ~] [['State of play'] ~]]] vase.bird]))
::
::  Get help on actions
    %help
    ::  Not sure how to get a newline character!
  =/  h1  "Available actions are: %challenge, %accept, %decline, %withdraw, %move, %pass, %resign, and %look."
  =/  h2  "Challenge: /ugo %challenge ~zod name=@t komi=@rs handicap=@ud size=@ud order=@tas"
  =/  h3  "           'order' is the order of play, and can be %random, %challenger, or %challenged"
  =/  h4  "           name, komi, handicap, size and order will use default values if not supplied"
  =/  h5  "Accept:    /ugo %accept ~zod"
  =/  h6  "Decline:   /ugo %decline ~zod"
  =/  h7  "Withdraw   /ugo %withdraw ~zod"
  =/  h8  "Move       /ugo %move game-id(e.g. ~2023.5.2..08.48.59..110d) [19 19]"
  =/  h9  "Pass       /ugo %pass game-id(e.g. ~2023.5.2..08.48.59..110d)"
  =/  h10  "Resign     /ugo %resign game-id(e.g. ~2023.5.2..08.48.59..110d)"
  =/  h11  "Look       /ugo %look game-id(e.g. ~2023.5.2..08.48.59..110d)"
  =/  h12  "           %look will return an image of the game board with the current state of play"
  =/  h13  "Help       /ugo %help"

  =/  help-p  `tape`(zing (limo [h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 h13 ~]))
  (pure:m !>([(crip help-p) vase.bird]))
==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::
::  you can return either a cord or a full formatted chat message
::+$  reply  $@(cord content:chat)
::
::::::::::: from sur/chat.hoon (check ~zod) ::::::::::::::::::::::::
::+$  content
::  $%  [%story p=story]
::      [%notice p=notice]
::  ==
::+$  story
::  (pair (list block) (list inline))
::::
::+$  block
::  $%  [%image src=cord height=@ud width=@ud alt=cord]
::      [%cite =cite]
::  ==
::+$  inline
::  $@  @t
::  $%  [%italics p=(list inline)]
::      [%bold p=(list inline)]
::      [%strike p=(list inline)]
::      [%blockquote p=(list inline)]
::      [%inline-code p=cord]
::      [%ship p=ship]
::      [%block p=@ud q=cord]
::      [%code p=cord]
::      [%tag p=cord]
::      [%link p=cord q=cord]
::      [%break ~]
::  ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
