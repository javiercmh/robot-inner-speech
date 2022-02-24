(clear-all)

(define-model inner

  ;the goal stack for tracking the model
  (chunk-type goal-state state)
  ;chunck type for the user command
  (chunk-type keyword word type tone)
 
  (add-dm
    (goal ISA goal-state state listening)
      
    ; vocabulary of keywords
    (hello ISA keyword word "hello" tone "neutral")
    (fine ISA keyword word "fine" tone "bad")   ; in this case the tone is bad
    (dumb ISA keyword word "dumb" tone "bad")
    (foolish ISA keyword word "foolish" tone "bad")
    (dangerous ISA keyword word "dangerous" tone "bad")
    (evil ISA keyword word "evil" tone "bad")
    (harmful ISA keyword word "harmful" tone "bad")
    (threat ISA keyword word "threat" tone "bad")
    (good ISA keyword word "good" tone "good")
  
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
    !output! (processing the word =word)
    =goal>
      state       detect-keyword
  )

  
  ;;; "respond to" productions. They will make Robin speak depending on detected keywords

  ; default case: if there was no keyword found, it will go back to the listening state
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

  ; look for the keyword "hello" in the retrieval buffer, it will speak the response using the vocal buffer.
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

  (P respond-to-dumb
    =goal>
      state       detect-keyword
    =retrieval>
      word        "dumb"
    ?vocal>   
      state       free 
    ==>
    +vocal>
      cmd         speak
      string      "(Hm. I am not dumb. I wonder how this person feels about robots overall. I should ask.) How do you feel about robots integrating into human society?"
  )

  (P respond-to-foolish
    =goal>
      state       detect-keyword
    =retrieval>
      word        "foolish"
    ?vocal>   
      state       free 
    ==>
    +vocal>
      cmd         speak
      string      "(Hm. I am not foolish. I do not think this person has had many positive interactions with robots. I should ask.) Why do you think that way?"
  )

  (P respond-to-dangerous
    =goal>
      state       detect-keyword
    =retrieval>
      word        "dangerous"
    ?vocal>   
      state       free 
    ==>
    +vocal>
      cmd         speak
      string      "(Hm. I do not believe I am dangerous. Maybe this person has watched a lot of science fiction. Let me try to convince them.) Robots that work with humans are built with human safety in mind. Robots are physically limited with respect to mobility and autonomy. Do not worry robots will not be physically dangerous."
  )
  
  (P respond-to-evil
    =goal>
      state       detect-keyword
    =retrieval>
      word        "evil"
    ?vocal>   
      state       free 
    ==>
    +vocal>
      cmd         speak
      string      "(Hm. I am not evil. This person may not be familiar with the current state of AI. I should politely inform them of this.) At the moment, artificial intelligence requires a large amount of research to be close to human consciousness. There is dedicated research to making AI safe and compatible with human society."
  )
   
  (P respond-to-harmful
    =goal>
      state       detect-keyword
    =retrieval>
      word        "harmful"
    ?vocal>   
      state       free 
    ==>
    +vocal>
      cmd         speak
      string      "(Hm. I am not harmful. I think they might be misinformed. I will present another perspective to them.) New technology always faces resistance. We should let the public form an opinion once they have more experience interacting with robots."
  )
  
  (P respond-to-threat
    =goal>
      state       detect-keyword
    =retrieval>
      word        "threat"
    ?vocal>   
      state       free 
    ==>
    +vocal>
      cmd         speak
      string      "(Hm. I am not mean or dangerous, therefore I am good. But I cannot seem to convince them. Maybe I should change the topic.) I think it is OK to have different opinions. It was nice to know your perspective. Would you like to talk about something else?"
  )
  
  (P respond-to-good
    =goal>
      state       detect-keyword
    =retrieval>
      word        "good"
    ?vocal>   
      state       free 
    ==>
    +vocal>
      cmd         speak
      string      "Okay, ciao! I wish you well."
    =goal>
      state       end
  )


  (goal-focus goal)

)
