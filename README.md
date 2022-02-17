# Robot inner speech

## About this fork

This fork modifies the repository built as additional material for the paper "What robots want? Hearing the inner voice of a robot" by Arianna Pipitone and Antonio Chella. Link to the original paper [here](https://www.sciencedirect.com/science/article/pii/S2589004221003394).

Fore more information, refer to the `Original_README.md` file.

## About the original repository

This project allows the robot to communicate its "thoughts" out loud while performing a task. This is defined as the robot's inner speech.

It is based on [ACT-R](http://act-r.psy.cmu.edu/) architecture.

The code is contained in the `INNER` folder.

- `INNER/inner_model.lisp`: contains the knowledge and the rules implementing the inner speech ability for robot in the cooperative scenario to set a table. 
- `INNER/demo.py`: contains a working example of the model in action.

You can try the inner speech model by just running ACT-R, or by integrating it to real robot.

## Running inner speech model in ACT-R (without robot)

To run ACT-R inner speech model, you need:
   - ACT-R architecture ([standalone version](http://act-r.psy.cmu.edu/software/)) 
   - Python 3

Put the ACT-R folder in your preferred location.

Clone this repository and put it into `~/Documents/`.

From a terminal, go to the ACT-R directory and run the software:

    $ ./run-act-r.command

If it fails, it might be that you need to give it executable permissions first:

    $ chmod +x run-act-r.command
 
Back to the repository folder, run the Python code `inner_speech.py`.

Now you can see the model working in your ACT-R console.


## Outputs

### inner_speech.py

    ACT-R connection has been started.
    #|Warning: Creating chunk START with no slots |#
    #|Warning: Creating chunk DETECTED-WORD with no slots |#
    #|Warning: Production WAIT-FOR-WORD makes a request to buffer AURAL without a query in the conditions. |#
    #|Warning: Creating chunk START with no slots |#
    #|Warning: Creating chunk DETECTED-WORD with no slots |#
    #|Warning: Production WAIT-FOR-WORD makes a request to buffer AURAL without a query in the conditions. |#
    Robin: Hi there! [waving hand]
    Human: hello robin
        0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
        0.000   AUDIO                  new-sound
        0.000   PROCEDURAL             CONFLICT-RESOLUTION
        0.300   AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
        0.300   PROCEDURAL             CONFLICT-RESOLUTION
        0.350   PROCEDURAL             PRODUCTION-FIRED WAIT-FOR-WORD
        0.350   PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
        0.350   PROCEDURAL             CLEAR-BUFFER AURAL
        0.350   AUDIO                  ATTEND-SOUND AUDIO-EVENT0-0
        0.350   PROCEDURAL             CONFLICT-RESOLUTION
        0.475   AUDIO                  SET-BUFFER-CHUNK AURAL WORD0
        0.475   PROCEDURAL             CONFLICT-RESOLUTION
        0.500   AUDIO                  new-sound
        0.525   PROCEDURAL             PRODUCTION-FIRED RETRIEVE-WORD
        0.525   PROCEDURAL             CLEAR-BUFFER AURAL
        0.525   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        0.525   DECLARATIVE            start-retrieval
        0.525   DECLARATIVE            RETRIEVED-CHUNK HELLO
        0.525   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL HELLO
        0.525   PROCEDURAL             CONFLICT-RESOLUTION
        0.575   PROCEDURAL             PRODUCTION-FIRED RESPOND-TO-HELLO
        0.575   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        0.575   PROCEDURAL             CLEAR-BUFFER VOCAL
        0.575   SPEECH                 SPEAK TEXT How are you today?
        0.575   PROCEDURAL             CONFLICT-RESOLUTION
        0.725   PROCEDURAL             CONFLICT-RESOLUTION
        0.775   AUDIO                  new-sound
    Robin: How are you today?
        0.775   PROCEDURAL             CONFLICT-RESOLUTION
        0.800   AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT1 NIL
        0.800   PROCEDURAL             CONFLICT-RESOLUTION
        1.075   PROCEDURAL             CONFLICT-RESOLUTION
        1.675   PROCEDURAL             CONFLICT-RESOLUTION
        1.675   ------                 Stopped because no events left to process
    #|Warning: Creating chunk START with no slots |#
    #|Warning: Creating chunk DETECTED-WORD with no slots |#
    #|Warning: Production WAIT-FOR-WORD makes a request to buffer AURAL without a query in the conditions. |#
    Human: fine
        0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
        0.000   AUDIO                  new-sound
        0.000   PROCEDURAL             CONFLICT-RESOLUTION
        0.300   AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
        0.300   PROCEDURAL             CONFLICT-RESOLUTION
        0.350   PROCEDURAL             PRODUCTION-FIRED WAIT-FOR-WORD
        0.350   PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
        0.350   PROCEDURAL             CLEAR-BUFFER AURAL
        0.350   AUDIO                  ATTEND-SOUND AUDIO-EVENT0-0
        0.350   PROCEDURAL             CONFLICT-RESOLUTION
        0.450   AUDIO                  SET-BUFFER-CHUNK AURAL WORD0
        0.450   PROCEDURAL             CONFLICT-RESOLUTION
        0.500   PROCEDURAL             PRODUCTION-FIRED RETRIEVE-WORD
        0.500   PROCEDURAL             CLEAR-BUFFER AURAL
        0.500   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        0.500   DECLARATIVE            start-retrieval
        0.500   DECLARATIVE            RETRIEVED-CHUNK FINE
        0.500   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL FINE
        0.500   PROCEDURAL             CONFLICT-RESOLUTION
        0.550   PROCEDURAL             PRODUCTION-FIRED RESPOND-TO-FINE
        0.550   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        0.550   PROCEDURAL             CLEAR-BUFFER VOCAL
        0.550   SPEECH                 SPEAK TEXT (Hm. This person does not appear interested. I do not want to be mean. I should ask them if something is wrong.) Is everything alright?
        0.550   PROCEDURAL             CONFLICT-RESOLUTION
        0.700   PROCEDURAL             CONFLICT-RESOLUTION
        0.750   AUDIO                  new-sound
    Robin (to himself): Hm. This person does not appear interested. I do not want to be mean. I should ask them if something is wrong.
    Robin: Is everything alright?
        0.750   PROCEDURAL             CONFLICT-RESOLUTION
        1.050   AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT1 NIL
        1.050   PROCEDURAL             CONFLICT-RESOLUTION
        7.500   AUDIO                  MOD-BUFFER-CHUNK AURAL-LOCATION
        7.500   PROCEDURAL             CONFLICT-RESOLUTION
        7.500   ------                 Stopped because no events left to process


### demo.py

    ACT-R connection has been started.
    #|Warning: Creating chunk DETECTED-ADVERBIAL-SOUND with no slots |#
    #|Warning: Creating chunk ENCODED-ADVERB with no slots |#
    #|Warning: Creating chunk DETECTED-LOCATION-SOUND with no slots |#
    #|Warning: Creating chunk EVAL-ETIQUETTE with no slots |#
    #|Warning: Creating chunk QUERING-ETIQUETTE-QUESTION with no slots |#
    #|Warning: Creating chunk ANSWERING-ETIQUETTE-QUESTION with no slots |#
    #|Warning: Creating chunk ANSWER-ETIQUETTE-QUESTION with no slots |#
    #|Warning: Creating chunk COMPLETE-ANSWER-ETIQUETTE-QUESTION with no slots |#
    #|Warning: Creating chunk REQUIRING-CONF with no slots |#
    #|Warning: Creating chunk ATTENDING-CONF with no slots |#
    #|Warning: Creating chunk DETECTING-CONF with no slots |#
    #|Warning: Creating chunk END with no slots |#
    #|Warning: Creating chunk DETECTED-ADVERBIAL-SOUND with no slots |#
    #|Warning: Creating chunk ENCODED-ADVERB with no slots |#
    #|Warning: Creating chunk DETECTED-LOCATION-SOUND with no slots |#
    #|Warning: Creating chunk EVAL-ETIQUETTE with no slots |#
    #|Warning: Creating chunk QUERING-ETIQUETTE-QUESTION with no slots |#
    #|Warning: Creating chunk ANSWERING-ETIQUETTE-QUESTION with no slots |#
    #|Warning: Creating chunk ANSWER-ETIQUETTE-QUESTION with no slots |#
    #|Warning: Creating chunk COMPLETE-ANSWER-ETIQUETTE-QUESTION with no slots |#
    #|Warning: Creating chunk REQUIRING-CONF with no slots |#
    #|Warning: Creating chunk ATTENDING-CONF with no slots |#
    #|Warning: Creating chunk DETECTING-CONF with no slots |#
    #|Warning: Creating chunk END with no slots |#
    put
    napkin
    near
    table
        0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
        0.000   AUDIO                  new-sound
        0.000   PROCEDURAL             CONFLICT-RESOLUTION
        0.300   AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
        0.300   PROCEDURAL             CONFLICT-RESOLUTION
        0.350   PROCEDURAL             PRODUCTION-FIRED DETECTED-COMMAND-SOUND
        0.350   PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
        0.350   PROCEDURAL             CLEAR-BUFFER IMAGINAL
        0.350   PROCEDURAL             CLEAR-BUFFER AURAL
        0.350   AUDIO                  ATTEND-SOUND AUDIO-EVENT0-0
        0.350   PROCEDURAL             CONFLICT-RESOLUTION
        0.425   AUDIO                  SET-BUFFER-CHUNK AURAL WORD0
        0.425   PROCEDURAL             CONFLICT-RESOLUTION
        0.475   PROCEDURAL             PRODUCTION-FIRED RETRIEVE-MEANING-VERB
        0.475   PROCEDURAL             CLEAR-BUFFER AURAL
        0.475   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        0.475   DECLARATIVE            start-retrieval
        0.475   DECLARATIVE            RETRIEVED-CHUNK PUT
        0.475   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL PUT
        0.475   PROCEDURAL             CONFLICT-RESOLUTION
        0.550   IMAGINAL               SET-BUFFER-CHUNK IMAGINAL CHUNK0
        0.550   PROCEDURAL             CONFLICT-RESOLUTION
        0.600   PROCEDURAL             PRODUCTION-FIRED ENCODE-COMMAND
        0.600   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        0.600   PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
        0.600   AUDIO                  FIND-SOUND
        0.600   AUDIO                  find-sound-failure
        0.600   PROCEDURAL             CONFLICT-RESOLUTION
        1.500   AUDIO                  new-sound
        1.500   PROCEDURAL             CONFLICT-RESOLUTION
        1.800   AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT1 NIL
        1.800   PROCEDURAL             CONFLICT-RESOLUTION
        1.850   PROCEDURAL             PRODUCTION-FIRED DETECTED-OBJECT-SOUND
        1.850   PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
        1.850   PROCEDURAL             CLEAR-BUFFER AURAL
        1.850   AUDIO                  ATTEND-SOUND AUDIO-EVENT1-0
        1.850   PROCEDURAL             CONFLICT-RESOLUTION
        2.000   AUDIO                  SET-BUFFER-CHUNK AURAL WORD1
        2.000   PROCEDURAL             CONFLICT-RESOLUTION
        2.050   PROCEDURAL             PRODUCTION-FIRED RETRIEVE-MEANING-OBJECT
        2.050   PROCEDURAL             CLEAR-BUFFER AURAL
        2.050   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        2.050   DECLARATIVE            start-retrieval
        2.050   DECLARATIVE            RETRIEVED-CHUNK NAPKIN
        2.050   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL NAPKIN
        2.050   PROCEDURAL             CONFLICT-RESOLUTION
        2.100   PROCEDURAL             PRODUCTION-FIRED ENCODE-OBJECT
        2.100   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        2.100   PROCEDURAL             CLEAR-BUFFER GOAL
        2.100   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK1
        2.100   PROCEDURAL             CONFLICT-RESOLUTION
        3.000   AUDIO                  new-sound
        3.000   PROCEDURAL             CONFLICT-RESOLUTION
        3.300   AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT2 NIL
        3.300   PROCEDURAL             CONFLICT-RESOLUTION
        3.350   PROCEDURAL             PRODUCTION-FIRED DETECTED-ADVERBIAL-SOUND
        3.350   PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
        3.350   PROCEDURAL             CLEAR-BUFFER AURAL
        3.350   AUDIO                  ATTEND-SOUND AUDIO-EVENT2-0
        3.350   PROCEDURAL             CONFLICT-RESOLUTION
        3.450   AUDIO                  SET-BUFFER-CHUNK AURAL WORD2
        3.450   PROCEDURAL             CONFLICT-RESOLUTION
        3.500   PROCEDURAL             PRODUCTION-FIRED RETRIEVE-MEANING-ADVERB
        3.500   PROCEDURAL             CLEAR-BUFFER AURAL
        3.500   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        3.500   DECLARATIVE            start-retrieval
        3.500   DECLARATIVE            RETRIEVED-CHUNK NEAR
        3.500   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL NEAR
        3.500   PROCEDURAL             CONFLICT-RESOLUTION
        3.550   PROCEDURAL             PRODUCTION-FIRED ENCODE-ADVERB
        3.550   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        3.550   PROCEDURAL             CLEAR-BUFFER GOAL
        3.550   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK2
        3.550   PROCEDURAL             CONFLICT-RESOLUTION
        4.500   AUDIO                  new-sound
        4.500   PROCEDURAL             CONFLICT-RESOLUTION
        4.800   AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT3 NIL
        4.800   PROCEDURAL             CONFLICT-RESOLUTION
        4.850   PROCEDURAL             PRODUCTION-FIRED DETECTED-LOCATION-SOUND
        4.850   PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
        4.850   PROCEDURAL             CLEAR-BUFFER AURAL
        4.850   AUDIO                  ATTEND-SOUND AUDIO-EVENT3-0
        4.850   PROCEDURAL             CONFLICT-RESOLUTION
        4.975   AUDIO                  SET-BUFFER-CHUNK AURAL WORD3
        4.975   PROCEDURAL             CONFLICT-RESOLUTION
        5.025   PROCEDURAL             PRODUCTION-FIRED RETRIEVE-MEANING-LOCATION
        5.025   PROCEDURAL             CLEAR-BUFFER AURAL
        5.025   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        5.025   DECLARATIVE            start-retrieval
        5.025   DECLARATIVE            RETRIEVED-CHUNK TABLE
        5.025   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL TABLE
        5.025   PROCEDURAL             CONFLICT-RESOLUTION
        5.075   PROCEDURAL             PRODUCTION-FIRED ENCODE-LOCATION
        5.075   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        5.075   PROCEDURAL             CLEAR-BUFFER VOCAL
        5.075   PROCEDURAL             CLEAR-BUFFER GOAL
        5.075   SPEECH                 SPEAK TEXT I have to place napkinnear the table
        5.075   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK3
        5.075   PROCEDURAL             CONFLICT-RESOLUTION
        5.225   PROCEDURAL             CONFLICT-RESOLUTION
        5.275   AUDIO                  new-sound
    I have to place napkinnear the table
        5.275   PROCEDURAL             CONFLICT-RESOLUTION
        5.575   AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT4 NIL
        5.575   PROCEDURAL             CONFLICT-RESOLUTION
        7.075   AUDIO                  MOD-BUFFER-CHUNK AURAL-LOCATION
        7.075   PROCEDURAL             CONFLICT-RESOLUTION
        7.125   PROCEDURAL             PRODUCTION-FIRED RETRIEVE-ETIQUETTE-QUESTION
        7.125   PROCEDURAL             CLEAR-BUFFER VOCAL
        7.125   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        7.125   SPEECH                 SPEAK TEXT What does the etiquette require?
        7.125   DECLARATIVE            start-retrieval
        7.125   DECLARATIVE            RETRIEVED-CHUNK P3
        7.125   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL P3
        7.125   PROCEDURAL             CONFLICT-RESOLUTION
        7.225   PROCEDURAL             CONFLICT-RESOLUTION
        7.275   AUDIO                  new-sound
    What does the etiquette require?
        7.275   PROCEDURAL             CONFLICT-RESOLUTION
        7.575   PROCEDURAL             CONFLICT-RESOLUTION
        8.875   PROCEDURAL             CONFLICT-RESOLUTION
        8.925   PROCEDURAL             PRODUCTION-FIRED RETRIVED-ETIQUETTE-QUESTION
        8.925   PROCEDURAL             CLEAR-BUFFER VOCAL
        8.925   SPEECH                 SPEAK TEXT The napkin has to stay on the plate
        8.925   PROCEDURAL             CONFLICT-RESOLUTION
        8.975   PROCEDURAL             PRODUCTION-FIRED INNER-W-QUESTION
        8.975   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        8.975   DECLARATIVE            start-retrieval
        8.975   DECLARATIVE            RETRIEVED-CHUNK ON
        8.975   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL ON
        8.975   PROCEDURAL             CONFLICT-RESOLUTION
        9.025   PROCEDURAL             PRODUCTION-FIRED MAKE-ETIQUETTE-QUESTION
        9.025   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        9.025   DECLARATIVE            start-retrieval
        9.025   DECLARATIVE            RETRIEVED-CHUNK P3
        9.025   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL P3
        9.025   PROCEDURAL             CONFLICT-RESOLUTION
        9.075   AUDIO                  new-sound
        9.075   PROCEDURAL             PRODUCTION-FIRED ANSWERING-ETIQUETTE-QUESTION
        9.075   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
    The napkin has to stay on the plate
        9.075   DECLARATIVE            start-retrieval
        9.075   DECLARATIVE            RETRIEVED-CHUNK PLATE
        9.075   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL PLATE
        9.075   PROCEDURAL             CONFLICT-RESOLUTION
        9.125   PROCEDURAL             PRODUCTION-FIRED ANSWER-ETIQUETTE-QUESTION
        9.125   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        9.125   DECLARATIVE            start-retrieval
        9.125   DECLARATIVE            RETRIEVED-CHUNK TABLE
        9.125   DECLARATIVE            SET-BUFFER-CHUNK RETRIEVAL TABLE
        9.125   PROCEDURAL             CONFLICT-RESOLUTION
        9.375   PROCEDURAL             CONFLICT-RESOLUTION
        10.825   PROCEDURAL             CONFLICT-RESOLUTION
        10.875   PROCEDURAL             PRODUCTION-FIRED COMPLETE-ANSWER-ETIQUETTE-QUESTION
        10.875   PROCEDURAL             CLEAR-BUFFER RETRIEVAL
        10.875   PROCEDURAL             CLEAR-BUFFER VOCAL
        10.875   SPEECH                 SPEAK TEXT It contravenes etiquette... The position has to be on plate and not table
        10.875   PROCEDURAL             CONFLICT-RESOLUTION
        10.975   PROCEDURAL             CONFLICT-RESOLUTION
        11.025   AUDIO                  new-sound
    It contravenes etiquette... The position has to be on plate and not table
        11.025   PROCEDURAL             CONFLICT-RESOLUTION
        11.325   PROCEDURAL             CONFLICT-RESOLUTION
        14.675   PROCEDURAL             CONFLICT-RESOLUTION
        14.725   PROCEDURAL             PRODUCTION-FIRED REQUIRED-CONFIRMATION
        14.725   PROCEDURAL             CLEAR-BUFFER VOCAL
        14.725   SPEECH                 SPEAK TEXT Would you like I do that action anyway?
        14.725   PROCEDURAL             CONFLICT-RESOLUTION
        14.825   PROCEDURAL             CONFLICT-RESOLUTION
        14.875   AUDIO                  new-sound
    Would you like I do that action anyway?
        14.875   PROCEDURAL             CONFLICT-RESOLUTION
        15.175   PROCEDURAL             CONFLICT-RESOLUTION
        16.825   PROCEDURAL             CONFLICT-RESOLUTION
        16.825   ------                 Stopped because no events left to process
    ATTENDING-CONF
    #|Warning: NIL does not name a chunk in the current model. |#
    None
    #|Warning: NIL does not name a chunk in the current model. |#
    None
        16.825   AUDIO                  new-sound
        16.825   PROCEDURAL             CONFLICT-RESOLUTION
        17.125   PROCEDURAL             CONFLICT-RESOLUTION
        17.125   ------                 Stopped because no events left to process





