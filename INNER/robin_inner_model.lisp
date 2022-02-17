(clear-all)

(define-model inner

  ;the goal stack for tracking the model
  (chunk-type goal-state state)
  ;chunck type for the user command
  (chunk-type keyword word type)
 
  (add-dm
    (goal ISA goal-state state start)
      
    ; vocabulary of keywords
    (hello ISA keyword word "hello")
    (fine ISA keyword word "fine")
  
  )

  ;; Productions

  ; wait for any word being spoken
  (P wait-for-word
    =goal>
      state       start
    =aural-location>
        isa       audio-event
        kind      word
        location  external
    ==>
    +aural>
      event       =aural-location
    =goal>
      state       detected-word
  )

  ; retrieve the word heard from the vocabulary of keywords
  (P retrieve-word
    =goal>
      state       detected-word
    =aural>
      ISA         sound
      content     =word
    ==>
    +retrieval>
      ISA         keyword
      word        =word
  )

  ; respond depending on the word heard, following the script

  (P respond-to-hello
    =goal>
      state       detected-word
    =retrieval>
      word        "hello"
    ?vocal>   
      state       free 
    ==>
    +vocal>
      cmd         speak
      string      "How are you today?"
  )

  (P respond-to-fine
    =goal>
      state       detected-word
    =retrieval>
      word        "fine"
    ?vocal>   
      state       free 
    ==>
    +vocal>
      cmd         speak
      string      "(Hm. This person does not appear interested. I do not want to be mean. I should ask them if something is wrong.) Is everything alright?"
  )
   
  (goal-focus goal)

)
