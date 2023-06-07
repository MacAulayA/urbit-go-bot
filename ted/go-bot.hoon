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
  ::  Write our own await-thread poke so that we can
  ::  get data from our custom poke in go-poke.hoon
  ::  from https://developers.urbit.org/reference/arvo/threads/examples/child-thread
  ::
++  custom-await-thread
  |=  [file=term args=vase]
  =/  m  (strand ,vase)
  ^-  form:m

  ;<  =bowl:spider  bind:m  get-bowl
  =/  tid  `@ta`(cat 3 'strand_' (scot %uv (sham file eny.bowl)))
  ;<  ~             bind:m  (watch-our /awaiting/[tid] %spider /thread-result/[tid])
  ;<  ~             bind:m  %-  poke-our
                            :*  %spider
                                %spider-start
                                !>([`tid.bowl `tid byk.bowl(r da+now.bowl) file args])
                            ==
  ;<  =cage         bind:m  (take-fact /awaiting/[tid])
  ;<  ~             bind:m  (take-kick /awaiting/[tid])
  ?+  p.cage  ~|([%strange-thread-result p.cage file tid] !!)
    %thread-done  
  (pure:m !>([%done '']))                                       :: return %done so we can ?+ against this easily 
    %thread-fail
  :: q.q.cage is a tang (list tank) in the form [%poke-fail <tang returned from thread>]
  =/  err  (tang q.q.cage)
  =/  err-tank  (tank (snag 1 err))
  =/  err-tape  (slag 5 ~(ram re err-tank))                     :: [%leaf "#### occupied ####"] to " occupied ####"
  =/  err-cord  (crip (scag (sub (lent err-tape) 5) err-tape))  :: " occupied ####" to 'occupied' 
  (pure:m !>([%fail err-cord]))                                 :: return failure reason rather than fail the thread
==
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
  !!
::  Need to ensure we're only interacting with our own ship's urbit-go
=/  text-tape  (trip text.bird)
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
  ;<  ~        bind:m  (poke [our %urbit-go-chat] [%urbit-go-action !>([%challenge name=name who=challenged komi=komi handicap=handicap size=size order=order])])  ::check this poke structure
  (pure:m !>([(crip ;:(weld "Go: " (scow %p challenged) " you have been challenged by " (scow %p our) "!")) vase.bird]))
::
::  Accept game invitation
    %accept
  =/  challenger-a  `@p`(slav %p (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  challenge=(list go-challenge:urbit-go)  bind:m  (scry [(list go-challenge:urbit-go)] `path`['gx' 'urbit-go-chat' 'challenges' 'noun' ~])
  =/  challenge-0  (snag 0 challenge)  :: first challenge in list
  =/  game-id=@dau  game-id.challenge-0
  ;<  ~        bind:m  (poke [our %urbit-go-chat] [%urbit-go-action !>([%accept-challenge who=challenger-a])])
  (pure:m !>([(crip ;:(weld "Go: game accepted. " (scow %p our) " plays " (scow %p challenger-a) " , with game-id: " (scow %da game-id))) vase.bird]))
::
::  Decline game invitation
    %decline
  =/  challenger-d  `@p`(slav %p (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  ~        bind:m  (poke [our %urbit-go-chat] [%urbit-go-action !>([%decline-challenge who=challenger-d])])
  (pure:m !>(['Go: game declined' vase.bird]))
::
::  Withdraw game invitation
    %withdraw
  =/  challenged-d  `@p`(slav %p (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  ~        bind:m  (poke [our %urbit-go-chat] [%urbit-go-action !>([%withdraw-challenge who=challenged-d])])
  (pure:m !>(['Go: challenge withdrawn' vase.bird]))
::
::  Make a move
    %move
  ::=/  game-id-m  `@dau`(slav %da (crip (weld "~" (parse-input "~" text-tape "error"))))
  =/  game-id-t  (crip (weld "~" (parse-input "~" text-tape "error")))
  =/  game-id-m  `@dau`(slav %da game-id-t)

  :: Scry for game data in order to get name of host
  ;<  game=go-game:urbit-go  bind:m  (scry [go-game:urbit-go] `path`['gx' 'urbit-go-chat' 'game' game-id-t 'noun' ~])

  :: get move position
  =/  posn-tape  (slag (need (find "[" text-tape)) text-tape)        :: position data "[x y]"
  =/  t  `tape`(flop (snip (flop (snip posn-tape))))                 :: "[x y]" to "x y"
  :: "x y" to [x y]
  =/  posn  [`@ud`(slav %ud (crip (scag (need (find " " t)) t))) `@ud`(slav %ud (crip (slag (add 1 (need (find " " t))) t)))] 

  ;<  our=@p        bind:m  get-our
  ;<  now=@da       bind:m  get-time
  :: Always send moves to the host, replicating the logic in the app.
  ;<  ted-res=vase  bind:m  (custom-await-thread %go-poke !>([host.game !>([%move id=game-id-m position=posn])]))
  =/  result  !<([term cord] ted-res)
  ?+    -.result  ~|  %thread-fail  !! 
      %done
    =/  move-msg  (crip ;:(weld "Go: " (scow %p our) " moved to " posn-tape))
    (pure:m !>([`reply`[%story [[[%image (crip ;:(weld "/~/scry/urbit-go-chat/game/" (scow %da game-id-m) "/" (scow %da now) ".svg")) 300 300 'go board'] ~] [[move-msg] ~]]] vase.bird]))
    ::
      %fail
    =/  move-msg  (crip ;:(weld "Go: Sorry " (scow %p our) ", illegal move: " (trip +.result)))
    (pure:m !>([move-msg vase.bird]))
  ==
::
::  Pass on your turn
    %pass
  =/  game-id-p  `@dau`(slav %da (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  ~        bind:m  (poke [our %urbit-go-chat] [%urbit-go-action !>([%pass id=game-id-p])])
  (pure:m !>([(crip (weld (weld "Go: " (scow %p our)) " passed.")) vase.bird]))
::
::  Resign the game
    %resign
  =/  game-id-r  `@dau`(slav %da (crip (weld "~" (parse-input "~" text-tape "error"))))

  ;<  our=@p   bind:m  get-our
  ;<  ~        bind:m  (poke [our %urbit-go-chat] [%urbit-go-action !>([%resign id=game-id-r])])
  (pure:m !>([(crip (weld (weld "Go: " (scow %p our)) " resigned.")) vase.bird]))
::
::  View the state of the board
    %look
  =/  game-id  `tape`(weld "~" (parse-input "~" text-tape "error"))

  :: Scry urbit-go directly, as I've added an svg return type
  ;<  now=@da   bind:m  get-time
  (pure:m !>([`reply`[%story [[[%image (crip ;:(weld "/~/scry/urbit-go-chat/game/" game-id "/" (scow %da now) ".svg")) 300 300 'go board'] ~] [['State of play'] ~]]] vase.bird]))
::
::  Get help on actions
    %help
  =/  h1  'Available actions are: %challenge, %accept, %decline, %withdraw, %move, %pass, %resign, and %look.'
  =/  h2  'Challenge: /ugo %challenge ~zod name=@t komi=@rs handicap=@ud size=@ud order=@tas'
  =/  h3  '           "order" is the order of play, and can be %random, %challenger, or %challenged'
  =/  h4  '           name, komi, handicap, size and order will use default values if not supplied'
  =/  h5  'Accept:    /ugo %accept ~zod'
  =/  h6  'Decline:   /ugo %decline ~zod'
  =/  h7  'Withdraw   /ugo %withdraw ~zod'
  =/  h8  'Move       /ugo %move game-id (e.g. ~2023.5.2..08.48.59..110d) [19 19]'
  =/  h9  'Pass       /ugo %pass game-id (e.g. ~2023.5.2..08.48.59..110d)'
  =/  h10  'Resign     /ugo %resign game-id (e.g. ~2023.5.2..08.48.59..110d)'
  =/  h11  'Look       /ugo %look game-id (e.g. ~2023.5.2..08.48.59..110d)'
  =/  h12  '           %look will return an image of the game board with the current state of play'
  =/  h13  'Help       /ugo %help'

  =/  b  break+~  
  =/  content-list  `(list inline:chat)`[h1 b b h2 b h3 b h4 b b h5 b b h6 b b h7 b b h8 b b h9 b b h10 b b h11 b h12 b b h13 ~]
  =/  repl  `reply`[%story *(list block:chat) content-list]

  (pure:m !>([repl vase.bird]))
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
