(clear-all)

(define-model inner

  ;the goal stack for tracking the model
  (chunk-type goal-state state)
  ;chunck type for the user command
  (chunk-type keyword word type)
 
  (add-dm
    (goal ISA goal-state state listening)
      
    ; vocabulary of keywords
    (hello ISA keyword word "hello" tone "neutral")
    (fine ISA keyword word "fine" tone "bad")   ; in this case the tone is bad
  
  )

  ;; Productions

  ; wait for any word being spoken
  ; if there is information in aural-location, it will store it in the aural buffer
  (P wait-for-word
    =goal>
      state       listening
    =aural-location>
        isa       audio-event
        kind      word
        location  external
    ==>
    +aural>
      event       =aural-location   ; attend-sound audio-event
    =goal>
      state       attend-sound
  )

  ; retrieve the word heard from the vocabulary of keywords
  ; if there's a word in the aural buffer, it will store it in the retrieval buffer
  (P retrieve-word
    =goal>
      state       attend-sound
    =aural>
      ISA         sound
      content     =word
    ==>
    +retrieval>
      ISA         keyword
      word        =word
    !output! (output is =word)
    =goal>
      state       detect-keyword
  )

  
  ;;; "respond to" productions. They will make Robin speak depending on detected keywords

  ; in case there was no keyword found, it will go back to the listening state
  (P no-keyword-found
    =goal>
      state       detect-keyword
    ?vocal>   
      state       free
    ?retrieval>
      buffer      failure
    ==>
    =goal>
      state       listening
  )

  ; if a keyword is found in he retrieval buffer, it will speak the response using the vocal buffer.
  (P respond-to-hello
    =goal>
      state       detect-keyword
    =retrieval>
      ISA         keyword
      word        "hello"
    ?vocal>   
      state       free 
    ==>
    =goal>
      state       listening
    +vocal>
      cmd         speak
      string      "How are you today?"
  )

  (P respond-to-fine
    =goal>
      state       detect-keyword
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
