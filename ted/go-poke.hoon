/-  spider
/+  *strandio
=,  strand=strand:spider
|%
  ::
  ::  custom-poke and custom-take-poke-ack in order
  ::  to return a vase rather than ~ 
  ::
++  custom-poke
  |=  [=dock cag=cage]
  =/  m  (strand ,vase)
  ^-  form:m
  =/  =card:agent:gall  [%pass /poke %agent dock %poke cag]
  ;<  ~       bind:m  (send-raw-card card)
  (custom-take-poke-ack /poke)
::
++  custom-take-poke-ack
  |=  =wire
  =/  m  (strand ,vase)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %agent * %poke-ack *]
    ?.  =(wire wire.u.in.tin)
      `[%skip ~]
    ?~  p.sign.u.in.tin
      `[%done !>(~)]
    =/  err-tank-li  (skim u.p.sign.u.in.tin unpack-err-msg)
    =/  error  `tank`(snag 0 err-tank-li)
    =/  error-tang  `tang`[error ~]
    `[%fail [%poke-fail error-tang]]
  ==
++  unpack-err-msg
  |=  =tank
  ^-  ?
  :: find the tank that is a leaf with a p starting with "####"
  ?.  =(-.tank %leaf)
    %.n
  =/  tank-tape  ~(ram re tank)
  ?~  (find "####" tank-tape)
    %.n
  %.y     
--
^-  thread:spider
|=  arg=vase
=/  args  !<([@p vase] arg)
=/  m  (strand ,vase)
^-  form:m

;<  v=vase  bind:m  (custom-poke [-.args %urbit-go] [%urbit-go-action +.args])
(pure:m !>(v))