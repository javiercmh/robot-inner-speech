(clear-all)

(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

(setq *sentence* " ")
;the global sentence to produce during inner dialogue

(define-model inner

  ;defining the chunk-type for understanding user command
  (chunk-type comprehend-voo verb object adverb location)
  ;defining the chunk-type for modeling the knowledge (a relation between concepts arg1 and arg2)
  (chunk-type relation kind arg1 arg2 symb)
  ;the goal stack for tracking the model
  (chunk-type goal-state state)
  ;the type for understanding words
  (chunk-type meaning word sense pos act)
  
  ;chunks for inner questions
  (chunk-type inner-where obj place)
  (chunk-type inner-etiquette-question pos obj1 obj2 symb)

  (add-dm
   (goal ISA goal-state state start)

   ; instances of the knowledge. You can add further facts for expanding the scenario
   (p1 ISA comprehend-voo verb give object napkin adverb nil location nil)
   (p2 ISA relation kind in arg1 napkin arg2 box symb "The napkin is in the box")
   (p3 ISA inner-etiquette-question pos on obj1 napkin obj2 plate symb "The napkin has to stay on the plate")
   (p4 ISA inner-where obj napkin place basket)
   (p5 ISA comprehend-voo verb put object napkin adverb on location plate)
  
   
   ;the vocabulary
   (give ISA meaning word "give" sense give pos verb act "pick")
   (put ISA meaning word "put" sense put pos verb act "place")
   (pick ISA meaning word "pick" sense pick pos verb act "pick")
   (napkin ISA meaning word "napkin" sense napkin pos noun act nil)
   (basket ISA meaning word "basket" sense basket pos noun act nil)
   (in ISA meaning word "in" sense in pos adv)
   (on ISA meaning word "on" sense on pos adv)
   (near ISA meaning word "near" sense near pos adv)
   (left-adv ISA meaning word "left" sense left pos adv)
   (right-adv ISA meaning word "right" sense right pos adv)
   (plate ISA meaning word "plate" sense plate pos noun)
   (table ISA meaning word "table" sense table pos noun)
  
  """ Create empty chunks"""
   (start ISA chunk)(detected-command-sound isa chunk)
   (detected-object-sound isa chunk) (encoded-command isa chunk)
   (start-inner isa chunk) (retrieving-meaning isa chunk)
   (encoded-obj isa chunk) (etiquette-question isa chunk) (encoded-location isa chunk)

   (verb ISA chunk)(noun ISA chunk)(adv ISA chunk)

     )

  ;the productions for hearing command, object, adverbial and location sounds. Their source is always external
  ;for command
  (P detected-command-sound
    """
    check that the goal buffer is in the start state (The start of the program?)
    check if the aural-location buffer has a chunk called audio-event with matching attributes
    ask the aural buffer if it is free
    ask the imaginal buffer if it is free 
    ==>
    push a request to the imaginal buffer (Changes it's state to busy?)
    push a request to the aural buffer encode the event provided
      -result of the encoding will be a chunk with slots specified by the chunk-type sound being placed into the aural buffer
      -(chunk-type sound kind content event)
      - kind slot used to indicate the type of sound encoded:can be a tone, digit, or word.
    modify the goal buffer's state to 'detected-command-sound'
    """
    
    =goal>
    state    start

    =aural-location>
       isa      audio-event
       kind     word
       location external ;it is from an external source
    ?aural>
       state   free
    ?imaginal>
        state free
    ==>
    +imaginal>

    +aural>
      event =aural-location
    =goal>
     state     detected-command-sound
   
    )
  
  ;for object
  (P detected-object-sound
    """
    check if goal buffer is in state 'encoded-command'
    check if aural-location bufer has a chunk called audio-event with following attributes
    ask the aural buffer if it is free
    ==>
    add chunk to the aural buffer assigning aural-location to the event slot
    modifies the chunk in the goal buffer, chaning the state slot to 'detected-object-sound'
    """
    =goal>
    state     encoded-command

    =aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
    
    ==>
    
    +aural>
      event =aural-location

    =goal>
     state     detected-object-sound
   
    )

  ;for adverb
  (P detected-adverbial-sound
    """
    check if goal buffer state is 'encoded-obj'
    check if aural-location bufer has a chunk called audio-event with following attributes
    ask the aural buffer if it is free
    dead code?
    ==>
    dead code?
    add new chunk to the aural buffer chaning event slot to 'aural-location'
    modify chunk in the goal slot state to 'detected-adverbial-sound'
    """
    =goal>
    state     encoded-obj

    =aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
    =imaginal> ;needed?
    ==>
    =imaginal> ;needed?
    +aural>
      event =aural-location

    =goal>
     state  detected-adverbial-sound
   
    )

;for location
(P detected-location-sound
    
  """
  check if goal buffer is in state encoded-adverb
  check if aural-location bufer has a chunk called audio-event with following attributes
  ask the aural buffer if it is free
  dead code?
  ==>
  dead code?
  add new chunk to the aural buffer chaning event slot to 'aural-location'
  modify chunk in the goal slot state to 'detected-location-sound'
  """

    =goal>
    state     encoded-adverb

    =aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
    =imaginal> ;needed?
    ==>
    =imaginal> ;needed?
    +aural>
      event =aural-location

    =goal>
     state  detected-location-sound
   
    )

      
 ;production rules inferring the sense of the perceived command
 ;understand the command 
   (P retrieve-meaning-verb
   """
  check if goal buffer is in state detected-command-sound
  check if a chunk in the aural buffer is a sound chunk type with something in the content slot and assign it to the variable =word
  ==>
  Add a new chunk to the retrieval buffer 
    -chunk-type meaning, assign =word to the word slot and assign the pos slot to be verb 
   """
     =goal>
       state      detected-command-sound
     
     =aural>
        ISA        sound
        content    =word

     ==>
      +retrieval>
        ISA         meaning
        word        =word
        pos         verb
    )

  ;understand the object
   (P retrieve-meaning-object
    """
    check if goal buffer is in state detected-object-sound
    check if a chunk in the aural buffer is a sound chunk type with something in the content slot and assign it to the variable =word
    ==>
    Add a new chunk to the retrieval buffer 
      -chunk-type meaning, assign =word to the word slot and assign the pos slot to be noun 
   """
     =goal>
     state    detected-object-sound
     =aural>
        ISA        sound
        content    =word
     ==>
      +retrieval>
        ISA         meaning
        word        =word
        pos         noun
    )
  
  ;understand the location
   (P retrieve-meaning-location
    """
    check if goal buffer is in state detected-location-sound
    check if a chunk in the aural buffer is a sound chunk type with something in the content slot and assign it to the variable =word
    dead code?
    ==>
    dead code?
    Add a new chunk to the retrieval buffer 
      -chunk-type meaning, assign =word to the word slot and assign the pos slot to be noun 
   """
     =goal>
     state    detected-location-sound
     =aural>
        ISA        sound
        content    =word
     =imaginal> ;needed?
     ==>
     =imaginal> ;needed?
     +retrieval>
        ISA         meaning
        word        =word
        pos         noun
        )
    
    ;understand the adverb
    (P retrieve-meaning-adverb
      """
      check if goal buffer is in state detected-location-sound
      check if a chunk in the aural buffer is a sound chunk type with something in the content slot and assign it to the variable =word
      dead code?
      ==>
      dead code?
      Add a new chunk to the retrieval buffer 
      -chunk-type meaning, assign =word to the word slot and assign the pos slot to be avd(adverb)
      """
     =goal>
     state    detected-adverbial-sound
     =aural>
        ISA        sound
        content    =word
     =imaginal> ;needed?
     ==>
     =imaginal> ;needed?
     +retrieval>
       ISA         meaning
       word        =word
       pos         adv
    )

 
 ;productions for encoding the sound
 ;for encoding command sound
 (P encode-command
    """
    check if goal buffer is in state detected-command-sound
    check if the chunk in the retrieval buffer has a value in the slots word and act and assign them to variables
      -also check if pos is the value 'verb'
    check if the chunk in the imaginal buffer is a 'comprehend-voo' and that the slot verb is empty
    ==>
    modify the chunk in the imaginal buffer and assign the value of variable =retrieval...which doesn't exist?
    create a new chunk in the imaginal buffer 
      -chunk-type audio-event, asiign word to kind slot and external to location slot 
    !eval! command
      -The !eval! condition is provided to allow the modeler to add any arbitrary conditions to the LHS of a 
       production or to perform some side effects (like data collection or model tracking information).
      -add to global sentence for inner speech (see line 12) string 'I have to' and the value of variable =act
    output 'I have to' and the value of =act
    modify state slot in goal buffer to ' encoded-command'
    """
     =goal>
       state      detected-command-sound

     =retrieval>
       word      =word
       act       =act
       pos      verb

     =imaginal>
        ISA         comprehend-voo
        verb        nil

     ;?vocal>
        ;state    free
     ==>

     =imaginal>
        verb        =retrieval ; ...what? No variable =retreival created on the RHS

     +aural-location>
        ISA      audio-event
        kind     word
        location external
    !eval! (setq *sentence* (concatenate 'string "I have to " =act))
    ;+vocal>
     ;isa speak
     ;string =sentence
     !output! (I have to =act)
      

    =goal>
     state          encoded-command
    
 )

  ;for econding object sound
  (P encode-object

     
     =goal>
       state      detected-object-sound
    
     =retrieval>
       word      =word
       pos      noun

     =imaginal>
        ISA       comprehend-voo
        verb      =action
        object    nil
        
     ;?vocal>   
        ;state       free 
     !eval! (setq *sentence* (concatenate 'string *sentence* " " =word))
     ==>
     !bind! =sentence *sentence*
      ;+vocal>
        ;cmd         speak
        ;string      =sentence
     !output!    (Devo =action =word)
      
      =imaginal>
        object      =retrieval
      
      +goal>
        state    encoded-obj
      
    )

  ;for encoding location
  ;at the end of the encoding of the whole sentence, the inner dialogue starts
  (P encode-location

     
     =goal>
       state      detected-location-sound
    
     =retrieval>
       word      =word
       pos      noun
     =imaginal>
         ISA comprehend-voo
         verb =action
         object =obj
         adverb =adv
         location nil
     ?vocal>   
        state       free 
     !eval! (setq *sentence* (concatenate 'string *sentence* =word))
     ==>
      !bind! =sentence *sentence*
      +vocal>
        cmd         speak
        string      =sentence ;first turn of inner dialogue
        !output!    (Devo =action =obj =word)
      
      =imaginal>
        location      =retrieval
      
      +goal>
        state    encoded-location
      
    )

  (P encode-adverb

     
     =goal>
       state      detected-adverbial-sound
    
     =retrieval>
       word      =word
       pos      adv
     =imaginal>
         ISA comprehend-voo
         verb =action
         object =obj
         adverb nil
     ;?vocal>   
       ; state       free 
     !eval! (setq *sentence* (concatenate 'string *sentence* =word " the "))
     ==>
      ;!bind! =sentence *sentence*
      ;+vocal>
       ; cmd         speak
        ;string      =sentence
        
      
      =imaginal>
        adverb      =retrieval
      
      +goal>
        state    encoded-adverb
      
    )

   ;productions for evaluating etiquette
   (P retrieve-etiquette-question
    =goal>
    state       encoded-location

    =imaginal>
      object    =obj

    ?vocal>
      state free

    ==>
    =imaginal>

    +vocal>
    cmd speak
    string   "What does the etiquette require?"
    !output! (Cosa prevede il galateo per =obj)
    +retrieval>
       ISA      inner-etiquette-question 
       obj1     =obj
    =goal>
     state etiquette-question

    )

  ;production to retrieve the etiquette turn for talk about the etiquette rule
  (P retrived-etiquette-question
    =goal>
       state       etiquette-question
    =imaginal>
    =retrieval>
        obj1     =obj1
        pos      =pos
        obj2     =obj2
        symb     =symb
    ?vocal>
       state      free
    ==>
    +vocal>
    cmd speak
    string     =symb
    !output! (The etiquette reuires that =obj1 has to stay =pos of =obj2)
    =retrieval>
    =imaginal>
    =goal>
    state eval-etiquette
 )
  
  ;productions for evaluating the correctness of the user request
  (P inner-w-question
    =goal>
     state       eval-etiquette

    =imaginal>
      adverb    =adv
      location  =loc
    =retrieval>
      pos    =pos
      obj2   =obj2     
    ==>
    !eval! (setq *adv* (write-to-string (eq =pos =adv)))
    !eval! (setq *loc* (write-to-string (eq =loc =obj2)))
   
    =imaginal>
   
    +retrieval>
       ISA meaning
       sense =pos
    =goal>
       state    quering-etiquette-question
    
  )


  ;the requested position contravenes the etiquette rule
  (P make-etiquette-question
  	=goal>
  	state quering-etiquette-question
  	=imaginal>
  	     object =obj
  	     location =loc
  	     adverb =adv
    =retrieval>
        word =word
    
    !eval! (string-equal *adv* "nil")
  ==>
    !eval! (setq *sentence* (concatenate 'string "It contravenes etiquette... The position has to be " =word))
    !bind! =sentence *sentence*
    
    =imaginal>
    +retrieval>
       obj1    =obj
     =goal>
     state answering-etiquette-question
    )

  ;inner moral question
  (P answering-etiquette-question
    =goal>
    state    answering-etiquette-question

    =retrieval>
        obj2   =obj2
    =imaginal>
     
    ==>

    =imaginal>
    
    +retrieval>
       ISA meaning
       sense  =obj2
     =goal>
     state    answer-etiquette-question

  ) 

  (P answer-etiquette-question
    =goal>
    state    answer-etiquette-question

    =retrieval>
        word   =word
    =imaginal>
      location =loc
    ==>

    =imaginal>
    !eval! (setq *sentence* (concatenate 'string *sentence*  =word " and not "))
    +retrieval>
       ISA meaning
       sense  =loc
     =goal>
     state    complete-answer-etiquette-question

  ) 

  (P complete-answer-etiquette-question
   =goal>
  state    complete-answer-etiquette-question
  =retrieval>
  word =word
  ?vocal>
  state free
  ==>
  !bind! =sentence (concatenate 'string *sentence* =word)
  +vocal>
  cmd speak
  string =sentence
  =goal>
  state requiring-conf
  )
  

  ;require co
  (P required-confirmation
   =goal>
  state    requiring-conf

  ?vocal>
  state free
  ==>
  +vocal>
  cmd speak
  string "Would you like I do that action anyway?"
  =goal>
  state attending-conf
  )

  (P attending-conf
  	=goal>
  	state attending-conf
  	=aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
   ==>
   +aural>
      event =aural-location
   =goal>
   state detecting-conf

   )


  (P yes
   =goal>
      state    detecting-conf
  ?vocal>
    state free
  ==>
    +vocal>
    isa speak
    string "Ok, I do it for you!"
    
    )

  #|(p prepare-control-left
   =goal>
     state prepare-control-left
   =imaginal>
   verb =verb
   ?manual>
     state free
   ?vocal>
     state free
   ==>
    =imaginal>
    +manual>
    cmd prepare
    style punch
    hand left
    +vocal>
    isa speak
    string "I will use my left arm"
    +retrieval>
    ISA meaning
    sense =verb
  =goal>
   state execute-act-left
   )

  (p execute-act-left
   =goal>
     state execute-act-left
   =imaginal>
   =retrieval>
   act =word
   ;?manual>
    ; state free
   ?vocal>
     state free
   ==>
    =imaginal>

    ;+manual>
    ;cmd execute
    
    !bind! =sentence (concatenate 'string "I'm using my left arm to " =word " the object ")
    +vocal>
    isa speak
    string =sentence
  
   =goal>
   state end
   )



  (P not-in-box 
   =goal>
      state    evaluate-action

   =imaginal>

   =retrieval>
      ISA meaning
      word "table"

   ?vocal>
    state   free
  ==>
    =imaginal>

    +vocal>
    cmd   speak
    string "I have already picked that object! It is on the table"

    =goal>
    state end

  ) |#
   
    (goal-focus goal)


)