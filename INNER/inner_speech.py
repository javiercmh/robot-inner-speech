import actr

actr.load_act_r_model("~/Documents/robot-inner-speech/INNER/robin_inner_model.lisp")

global response, response_time
response = ''
response_time = False


# def speak(string):
#     # make_robin_speak(string)
#     print("Robin:", string)

def robot_speaks(model=None, string=''):
    """This function is called when the model is supposed to speak. 
    The string is the text to be spoken.
    """

    print("Robin:", string)


def inner_speech():
    """ Run the ACT-R model.

    It starts with the robot saying "hi"
    """

    actr.reset()
    actr.set_parameter_value(":sound-decay-time", 0.4)  #0.3 thread 1
    actr.set_parameter_value(":save-audicon-history", True)

    actr.add_command("inner-speech-response", robot_speaks,
                     "Inner speech model response")
    actr.monitor_command("output-speech", "inner-speech-response")

    actr.install_device(["speech", "microphone"])

    robot_speaks(string='Hi there! [waving hand]')
    
    # we get our response (but in this case we hardcode it)
    # response = get_human_input()
    text = "hello robin"
    print("Human:", text)

    # give text to the model word by word
    onset = 0
    for word in text.split():
        actr.new_word_sound(word, onset)
        onset = onset + 0.5  # separate words by 0.5 seconds (in model-time)

    actr.run(30)    # run model for up to 30 seconds
    
    return  # break here for now

    # actr.set_chunk_slot_value(actr.buffer_read('goal'), "state", "done")
    print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))     # with this line we can get info from a buffer.
        
    actr.new_word_sound("yes")
    
    print(actr.chunk_slot_value(actr.buffer_read('aural'), "content"))
    
    actr.run(10)

    # actr.remove_command_monitor("output-speech", "inner-speech-response")
    # actr.remove_command("inner-speech-response")

inner_speech()