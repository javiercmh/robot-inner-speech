(clear-all)

(defun concatString (list)
;;   "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result " "))
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
  
;;   """ Create empty chunks"""
   (start ISA chunk)(detected-command-sound isa chunk)
   (detected-object-sound isa chunk) (encoded-command isa chunk)
   (start-inner isa chunk) (retrieving-meaning isa chunk)
   (encoded-obj isa chunk) (etiquette-question isa chunk) (encoded-location isa chunk)

   (verb ISA chunk)(noun ISA chunk)(adv ISA chunk)

     )

  ;the productions for hearing command, object, adverbial and location sounds. Their source is always external
  ;for command
  (P detected-command-sound
    ;; """
    ;; check that the goal buffer is in the start state (The start of the program?)
    ;; check if the aural-location buffer has a chunk called audio-event with matching attributes
    ;; ask the aural buffer if it is free
    ;; ask the imaginal buffer if it is free 
    ;; ==>
    ;; push a request to the imaginal buffer (Changes it's state to busy?)
    ;; push a request to the aural buffer encode the event provided
    ;;   -result of the encoding will be a chunk with slots specified by the chunk-type sound being placed into the aural buffer
    ;;   -(chunk-type sound kind content event)
    ;;   - kind slot used to indicate the type of sound encoded:can be a tone, digit, or word.
    ;; modify the goal buffer's state to 'detected-command-sound'
    ;; """
    
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
    ;; """
    ;; check if goal buffer is in state 'encoded-command'
    ;; check if aural-location bufer has a chunk called audio-event with following attributes
    ;; ask the aural buffer if it is free
    ;; ==>
    ;; add chunk to the aural buffer assigning aural-location to the event slot
    ;; modifies the chunk in the goal buffer, chaning the state slot to 'detected-object-sound'
    ;; """
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
    ;; """
    ;; check if goal buffer state is 'encoded-obj'
    ;; check if aural-location bufer has a chunk called audio-event with following attributes
    ;; ask the aural buffer if it is free
    ;; dead code?
    ;; ==>
    ;; dead code?
    ;; add new chunk to the aural buffer chaning event slot to 'aural-location'
    ;; modify chunk in the goal slot state to 'detected-adverbial-sound'
    ;; """
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
    
;;   """
;;   check if goal buffer is in state encoded-adverb
;;   check if aural-location bufer has a chunk called audio-event with following attributes
;;   ask the aural buffer if it is free
;;   dead code?
;;   ==>
;;   dead code?
;;   add new chunk to the aural buffer chaning event slot to 'aural-location'
;;   modify chunk in the goal slot state to 'detected-location-sound'
;;   """

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
;;    """
;;   check if goal buffer is in state detected-command-sound
;;   check if a chunk in the aural buffer is a sound chunk type with something in the content slot and assign it to the variable =word
;;   ==>
;;   Add a new chunk to the retrieval buffer 
;;     -chunk-type meaning, assign =word to the word slot and assign the pos slot to be verb 
;;    """
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
;;     """
;;     check if goal buffer is in state detected-object-sound
;;     check if a chunk in the aural buffer is a sound chunk type with something in the content slot and assign it to the variable =word
;;     ==>
;;     Add a new chunk to the retrieval buffer 
;;       -chunk-type meaning, assign =word to the word slot and assign the pos slot to be noun 
;;    """
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
;;     """
;;     check if goal buffer is in state detected-location-sound
;;     check if a chunk in the aural buffer is a sound chunk type with something in the content slot and assign it to the variable =word
;;     dead code?
;;     ==>
;;     dead code?
;;     Add a new chunk to the retrieval buffer 
;;       -chunk-type meaning, assign =word to the word slot and assign the pos slot to be noun 
;;    """
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
    ;;   """
    ;;   check if goal buffer is in state detected-location-sound
    ;;   check if a chunk in the aural buffer is a sound chunk type with something in the content slot and assign it to the variable =word
    ;;   dead code?
    ;;   ==>
    ;;   dead code?
    ;;   Add a new chunk to the retrieval buffer 
    ;;   -chunk-type meaning, assign =word to the word slot and assign the pos slot to be avd(adverb)
    ;;   """
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
    ;; """
    ;; check if goal buffer is in state detected-command-sound
    ;; check if the chunk in the retrieval buffer has a value in the slots word and act and assign them to variables
    ;;   -also check if pos is the value 'verb'
    ;; check if the chunk in the imaginal buffer is a 'comprehend-voo' and that the slot verb is empty
    ;; check staus of vocal buffer if state is free
    ;; ==>
    ;; modify the chunk in the imaginal buffer and assign the value of variable =retrieval...which doesn't exist?
    ;; create a new chunk in the imaginal buffer 
    ;;   -chunk-type audio-event, asiign word to kind slot and external to location slot 
    ;; !eval! command
    ;;   -The !eval! condition is provided to allow the modeler to add any arbitrary conditions to the LHS of a 
    ;;    production or to perform some side effects (like data collection or model tracking information). - ACT-R reference guide
    ;;   -setq - sets value of a variable, *sentence* in this case
    ;;   -add to global sentence for inner speech (see line 12) string 'I have to' and the value of variable =act
    ;; output 'I have to' and the value of =act
    ;; modify state slot in goal buffer to ' encoded-command'
    ;; """
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
    ;; """
    ;; check if goal buffer is in state detected-object-sound
    ;; check if the chunk in the retrieval buffer has a value in the slot word and assign it to =word; and slot pos has value of 'noun'
    ;; check if the chunk in the imaginal buffer is a 'comprehend-voo', verb has a value and assign value to =action, and that the slot object is empty
    ;; check staus of vocal buffer if state is free
    ;; eval command,set variable sentence to be a string of variable sentence, a space ,and value of =word
    ;; ==>
    ;; !bind! command 
    ;;  - !bind! the return value of the evaluation is saved in a variable of the production which can then be used like other variables in 
    ;;   the production with the exception of not being able to use it in a slot name position. - ACT-R reference guide
    ;;  - binds the variable *sentence* to variable =sentence
    ;; request the vocal buffer to change the value of cmd to speak and the vaue of string to new =sentence variable
    ;; output - 'I must' and the variables =action and =word
    ;; Modify the chunk in the imaginal buffer; slot object to variable retrieval
    ;; Add new chunk to the goal buffer; state slot to encoded-obj
    
    ;; """
     
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
     !output!    (I have to =action =word)
      
      =imaginal>
        object      =retrieval ; 2, No variable =retreival created on the RHS
      
      +goal>
        state    encoded-obj
      
    )

  ;for encoding location
  ;at the end of the encoding of the whole sentence, the inner dialogue starts
  (P encode-location
    ;;  """
    ;;  check if goal buffer is in state detected-object-sound
    ;;  check if the chunk in the retrieval buffer has a value in the slot word and assign it to =word; and slot pos has value of 'noun'
    ;;  check if the chunk in the imaginal buffer is a 'comprehend-voo'
    ;;   -verb has a value and assign value to =action 
    ;;   -object has a value and assign value to =obj
    ;;   -adverb has a value and assign value to =adv
    ;;   -location is empty
    ;;  check staus of vocal buffer if state is free
    ;;  eval command,set variable sentence to be a string of variable sentence and value of =word
    ;;  ==>
    ;;  binds the variable *sentence* to variable =sentence
    ;;  Add new chunk to the vocal buffer to change the value of cmd to speak and the vaue of string to new =sentence variable
    ;;  output - 'I must' and the variables =action, =obj, and =word
    ;;  Modify the chunk in the imaginal buffer; slot object to variable retrieval
    ;;  Add new chunk to the goal buffer; state slot to encoded-location
    ;;  """
     
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
        !output!    (I have to =action =obj =word)
      
      =imaginal>
        location      =retrieval
      
      +goal>
        state    encoded-location
      
    )

  (P encode-adverb
    ;; """
    ;; check if goal buffer is in state detected-adverbial-sound
    ;; check if the chunk in the retrieval buffer has a value in the slot word and assign it to =word; and slot pos has value of 'adv'
    ;; check if the chunk in the imaginal buffer is a 'comprehend-voo'
    ;;   -verb has a value and assign value to =action 
    ;;   -object has a value and assign value to =obj
    ;;   -adverb is empty
    ;; check staus of vocal buffer if state is free
    ;; eval command,set variable sentence to be a string of variable sentence and value of =word and ' the '
    ;; ==>
    ;; binds the variable *sentence* to variable =sentence
    ;; Add new chunk to the vocal buffer to change the value of cmd to speak and the vaue of string to new =sentence variable
    ;; Modify the chunk in the imaginal buffer; slot adverb to variable retrieval
    ;; Add new chunk to the goal buffer; state slot to encoded-adverb
    ;; """
     
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
    ;; """
    ;; Check if chunk in goal buffer has slot state with value 'encoded-location'
    ;; Check if chunk in imaginal buffer has slot object with a value and assign it to =obj
    ;; Check status of vocal buffer- if state free
    ;; ==>
    ;; dead code?
    ;; Change chunk in the vocal buffer 
    ;;    -cmd to speak
    ;;    -string to 'What does the etiquette require?'
    ;; !output! 'The etiquette requires' + variable obj
    ;; Change chunk in the retrieval buffer to be a inner-etiquette-question chunk-type with slot obj1 being assigned =obj's value 
    ;; Modify chunk in goal buffer: state to etiquette-question
    ;; """
      
    =goal>
    state       encoded-location

    =imaginal>
      object    =obj

    ?vocal>
      state free

    ==>
    =imaginal> ;dead code?

    +vocal>
    cmd speak
    string   "What does the etiquette require?"
    !output! (What is the etiquette required for =obj)
    +retrieval>
       ISA      inner-etiquette-question 
       obj1     =obj
    =goal>
     state etiquette-question

    )

  ;production to retrieve the etiquette turn for talk about the etiquette rule
  (P retrived-etiquette-question
    ;;  """
    ;;  check if chunk in goal buffer has slot state assigned as etiquette-question
    ;;  dead code?
    ;;  Check if chunk in retrieval buffer has values in the given slots and assign them to variables
    ;;  check if vocal buffer is in state free
    ;;  ==>
    ;;  Add chunk to vocal buffer changing cmd slot to speak and string to =symb
    ;;  output a string of text and variables
    ;;  Modify chunk in goal buffer: state to eval-etiquette
    ;;  """
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
    !output! (The etiquette requires that =obj1 stays =pos =obj2)
    =retrieval> ;dead code?
    =imaginal> ;dead code?
    =goal>
    state eval-etiquette
 )
  
  ;productions for evaluating the correctness of the user request
  (P inner-w-question
    ;;  """
    ;;  Check if chunk in the goal buffer has slot state as eval-etiquette
    ;;  Check if chunk in imaginal buffer has values in slots adverb and location, set to variables
    ;;  Check if chunk in retrieval buffer has values in slots pos and obj2, set to variables
    ;;  ==>
    ;;  create variable *adv* and assign value of the result of checking if =pos and =adv are equal as a string
    ;;  create variable *loc* and assign value of the result of checking if =loc and =obj2 are equal as a string
    ;;  Change the chunk in the retrieval buffer to be a meaning chunk type with value of sense being set to =pos
    ;;  Modify chunk in the goal buffer: change slot state to quering-etiquette-question
    ;;  """
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
   
    =imaginal> ; dead code?
   
    +retrieval>
       ISA meaning
       sense =pos
    =goal>
       state    quering-etiquette-question
    
  )


  ;the requested position contravenes the etiquette rule
  (P make-etiquette-question
  	;; """
    ;; Check if chunk in goal buffer has slot state assigned to 'quering-etiquette-question'
    ;; Check if chunk in imaginal buffer has slots object, location, and adverb with values and set them to variables
    ;; Check if chunk in retrieval buffer has slot word and assign value to variable =word
    ;; Evaluate if the value of *adv* and 'nil' are equal as strings
    ;; ==>
    ;; Change value of variable *sentence* to be string 'It contravenes etiquette... The position has to be ' and value of =word
    ;; binds the variable *sentence* to variable =sentence
    ;; Add chunk to the retrieval buffer with slot obj1 assigned value of =obj
    ;; Modify chunk in the goal buffer: change slot state to answering-etiquette-question
    ;; """
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
    
    =imaginal> ;dead code?
    +retrieval>
       obj1    =obj
     =goal>
     state answering-etiquette-question
    )

  ;inner moral question
  (P answering-etiquette-question
    ;; """
    ;; Check if chunk in goal buffer has slot state assigned to 'answering-etiquette-question'
    ;; Check if chunk in retrieval buffer has slot obj2 and assign value to variable =obj2
    ;; Evaluate if the value of *adv* and 'nil' are equal as strings
    ;; ==>
    ;; Add chunk to the retrieval buffer as meaning chunk-type with slot sense assigned value of =obj2
    ;; Modify chunk in the goal buffer: change slot state to answer-etiquette-question
    ;; """
    =goal>
    state    answering-etiquette-question

    =retrieval>
        obj2   =obj2
    =imaginal>; dead code?
     
    ==>

    =imaginal> ; dead code?
    
    +retrieval>
       ISA meaning
       sense  =obj2
     =goal>
     state    answer-etiquette-question

  ) 

  (P answer-etiquette-question
    ;; """
    ;; Check if chunk in goal buffer has slot state assigned to 'answer-etiquette-question'
    ;; Check if chunk in retrieval buffer has slot word and assign value to variable =word
    ;; Check if chunk in imaginal buffer has slot location and assign value to variable =loc
    ;; ==>
    ;; Change value of variable *sentence* to be string of the value of *sentence* and value of =word + ' and not '
    ;; Add chunk to the retrieval buffer as chunk-type meaning with slot sense assigned value of =loc
    ;; Modify chunk in the goal buffer: change slot state to complete-answer-etiquette-question    
    ;; """
     
    =goal>
    state    answer-etiquette-question

    =retrieval>
        word   =word
    =imaginal>
      location =loc
    ==>

    =imaginal>;dead code?
    !eval! (setq *sentence* (concatenate 'string *sentence* " " =word " and not "))  ;; this extra " " should split the word "onplate"
    +retrieval>
       ISA meaning
       sense  =loc
     =goal>
     state    complete-answer-etiquette-question

  ) 

  (P complete-answer-etiquette-question
;;    """ 
;;    check if chunk in goal buffer has slot state assigned as complete-answer-etiquette-question
;;    Check if chunk in retrieval buffer has a chunk with value in the word slot and assign to variable =word
;;    check if vocal buffer is in state free
;;    ==>
;;    binds the concatenation of variable *sentence* and =word to variable =sentence
;;    Add chunk to vocal buffer changing cmd slot to speak and string to =sentence
;;    Modify chunk in goal buffer: change slot state to requiring-conf
;;    """
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
;;   """
;;   Check if there is a chunk in the goal buffer with slot state assigned as requiring-conf
;;   Check if vocal buffer is in state free
;;   ==>
;;   Add new chunk to the vocal buffer with slot cmd set to speak and slot string assigned 'Would you like I do that action anyway?'
;;   Modify chunk in goal buffer: change slot state to attending-conf
;;   """ 
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
  	;; """
    ;; Check if chunk in the goal buffer is in state attending-conf
    ;; Check if aural-location buffer has chunk-type audio-event with the slot kind having a word value and slot location external as value 
    ;; Check if aural buffer is in state free
    ;; ==>
    ;; Add new chunk to aural buffer with slot event set equal to variable =aural-location
    ;; Modify chunk in goal buffer: change slot state to detecting-conf
    ;; """
    =goal>
  	  state   attending-conf
  	=aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
   ==>
   +aural>
      event =aural-location ; Third time this happens, maybe setting a value to slot from the buffer directly?
   =goal>
      state detecting-conf
    ;; !output! (I =word aaaaaaa)
   )


  (P yes
    ;; """
    ;; check if chunk in goal buffer has slot state with vale detecting-conf
    ;; check if vocal buffer is in state free
    ;; ==>
    ;; Add new chunk to vocal buffer as speak chunk-type with slot string assigned 'Ok, I do it for you!'
    ;; """
   =goal>
      state    detecting-conf
  ?vocal>
    state free
  ==>
    +vocal>
    isa speak
    string "Ok, I do it for you!"
    =goal>
      state end
    )

  #|(p prepare-control-left
;;   """
;;   Check if chunk in the goal buffer has slot state set to prepare-control-left
;;   check if chunk in the imaginal buffer has a value in slot verb and assign to variable =verb
;;   check if manual buffer is in state free
;;   check if vocal buffer is in state free
;;   ==>
;;   Add new chunk to manual buffer with cmd set to prepare, style set to punch, and hand set to left
;;   Add new chunk to vocal buffer as chunk-type speak with slot string set to 'I will use my left arm'
;;   Add new chunk to the retrieval buffer as chunk-type meaning with slot sense set to value of variable =verb
;;   Modify chunk in goal buffer: slot stae to execute-act-left
;;   """
   =goal>
     state prepare-control-left
   =imaginal>
   verb =verb
   ?manual>
     state free
   ?vocal>
     state free
   ==>
    =imaginal> ;dead code?
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
;;   """
;;   Check if chunk in the goal buffer has slot state set to execute-act-left
;;   Check if chunk in the retrieval buffer has a valus in slot act and assign to variable =word
;;   check if manual buffer is in state free
;;   check if vocal buffer is in state free
;;   ==>
;;   Add new chunk to manual buffer set slot cmd to execute
;;   Assign the concatenation of '"I'm using my left arm to " =word " the object "' as a string to variable =sentence
;;   Add new chunk to the vocal buffer as chunk-type speak with string slot set to value of =sentence
;;   Modify chunk in goal buffer: slot state to end
;;   """
   =goal>
     state execute-act-left
   =imaginal> ;dead code?
   =retrieval>
   act =word
   ;?manual>
    ; state free
   ?vocal>
     state free
   ==>
    =imaginal> ;dead code?

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
;;   """
;;   Check if chunk in goal buffer has slot state assigned as evaluate-action
;;   Check if there is a chunk in the retrieval buffer that is a chunk-type meaning wiht value 'table' in slot word
;;   Check if voal buffer is in state free
;;   ==>
;;   Add new chunk to the vocal buffer with slot cmd set to speak and slot string set to 'I have already picked that object! It is on the table'
;;   Modify chunk in goal buffer: slot state to end
;;   """
   =goal>
      state    evaluate-action

   =imaginal>;dead code?

   =retrieval>
      ISA meaning
      word "table"

   ?vocal>
    state   free
  ==>
    =imaginal>;dead code?

    +vocal>
    cmd   speak
    string "I have already picked that object! It is on the table"

    =goal>
    state end

  ) |#
   
    (goal-focus goal)


)
