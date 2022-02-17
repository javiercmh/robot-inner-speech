import actr

actr.load_act_r_model("~/Documents/robot-inner-speech/INNER/robin_inner_model.lisp")

global response, response_time
response = ''
response_time = False


def robin_speaks(model=None, string=''):
    """Gets what Robin is supposed to say.
    """
    print("Robin:", string)


def human_speaks(string):
    """Takes human speech and sends it to the model word by word.
    """
    onset = actr.get_time(True)/1000
    for word in string.split():
        actr.new_word_sound(word, onset)
        onset = onset + 0.5  # separate words by 0.5 seconds (in model-time)
    print("Human:", string)


def inner_speech():
    """ Run the ACT-R model.

    It starts with the robot saying "hi"
    """

    actr.reset()
    actr.set_parameter_value(":sound-decay-time", 0.4)  #0.3 thread 1
    actr.set_parameter_value(":save-audicon-history", True)

    actr.add_command("inner-speech-response", robin_speaks,
                     "Inner speech model response")
    actr.monitor_command("output-speech", "inner-speech-response")

    actr.install_device(["speech", "microphone"])

    robin_speaks(string='Hi there! [waving hand]')
    
    # we get our response (but in this case we hardcode it)
    # response = get_human_input()
    text = "hello robin"
    human_speaks(text)

    actr.run(30)    # run model for up to 30 seconds
    print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))
    return  # break here for now

    # actr.set_chunk_slot_value(actr.buffer_read('goal'), "state", "done")
    print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))
        
    actr.new_word_sound("yes")
    
    print(actr.chunk_slot_value(actr.buffer_read('aural'), "content"))
    
    actr.run(10)

    # actr.remove_command_monitor("output-speech", "inner-speech-response")
    # actr.remove_command("inner-speech-response")

inner_speech()