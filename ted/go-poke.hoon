/-  spider
/+  *strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m

:: What if we watch for the poke from this separate thread??
::~&  "Running go-poke, about to take-poke"
::;<  our=@p  bind:m  get-our
::;<  =vase   bind:m  ((handle ,vase) (take-poke %urbit-go-action))
::(pure:m vase)

:: therefore...
;<  our=@p  bind:m  get-our
~&  "[go-poke]: have our, poking %urbit-go"
;<  ~       bind:m  (poke [our %urbit-go] [%urbit-go-action arg])
(pure:m !>([%complete]))