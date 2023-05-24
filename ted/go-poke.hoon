/-  spider
/+  *strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m

:: send a cage (%blah !>(thing)) through, then pass it directly to the 
:: poke strand??

:: poke takes [dock cage]
:: dock doesn't need to be passed in, as it's [our %urbit-go]
:: we don't need the @tas for the cage either as it's always %urbit-go-action
:: so we just need a vase !>([%move ....]), which we pass in.

:: What do we return though??  A vase or a cage seems sensible, but of what??
::(pure:m !>(%complete)) :: so as not to confuse with %done
:: if the poke doesn't fail the caller will then get a %complete, and can
:: send the correct message to %gato


:: Hm, what if we watch for the poke from this separate thread??
::~&  "Running go-poke, about to take-poke"
::;<  our=@p  bind:m  get-our
::;<  =vase   bind:m  ((handle ,vase) (take-poke %urbit-go-action))
::~&  "Taking poke, returning %complete"
::(pure:m vase)

:: therefore...
;<  our=@p  bind:m  get-our
~&  "[go-poke]: have our, poking %urbit-go"
;<  ~       bind:m  (poke [our %urbit-go] [%urbit-go-action arg])
(pure:m !>([%complete]))