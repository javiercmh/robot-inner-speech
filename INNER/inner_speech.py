import logging
import re
import actr
import speech_recognition as sr
import libfinroc_plugins_interface_api_finroc_interface as finroc


logging.basicConfig(
    # filename='output.log', 
    format='%(asctime)s %(message)s',
    level=logging.DEBUG,
    handlers=[
        logging.FileHandler("debug.log"),
        logging.StreamHandler()
    ])


class Mind():
    def __init__(self, mic_index=0, sample_rate=16000, engine='sphinx'):
        """Initializes the object with the model, finroc, and mic settings.
        Parameters:
            actr: ACT-R model
            finroc: Finroc object
            mic_index: microphone device index
            sample_rate: sample rate of microphone
            engine: speech recognition engine (sphinx or google)
        """
        self.time = 0
        self.finroc = finroc
        self.mic_index = mic_index
        self.sample_rate = sample_rate
        self.engine = engine

        # load model
        actr.load_act_r_model("~/Documents/robot-inner-speech/INNER/robin_inner_model.lisp")
        logging.debug('[Inner speech model initialized]')

        # initialize finroc
        interface = finroc.CreateInterfaceModule("actr_interface", None)
        finroc.Initialize("ACTR Inner Speech")
        self.robot_inner_dialog = interface.CreateOutputPort("robot_inner_dialog")
        self.robot_regular_dialog = interface.CreateOutputPort("robot_regular_dialog")


    def reset_actr(self):
        self.time = actr.get_time(True) / 1000

        actr.remove_command_monitor("output-speech", "inner-speech-response")
        actr.remove_command("inner-speech-response")

        actr.reset()
        actr.set_parameter_value(":sound-decay-time", 0.3)

        actr.add_command("inner-speech-response", self.robin_speaks,
                        "Inner speech model response")
        actr.monitor_command("output-speech", "inner-speech-response")

        actr.install_device(["speech", "microphone"])

    def robin_speaks(self, string=''):
        """Gets what Robin is supposed to say.
        """
        # separate inner- from regular speech
        if '(' in string:
            inner, regular = string.split(') ')
            inner = inner.replace('(', '')

            # send speech to finroc
            self.robot_inner_dialog.Publish(inner, 0)
            self.robot_regular_dialog.Publish(regular, 0)

            logging.debug(f"Robin (to themself): {inner}")
            logging.debug(f"Robin: {regular}")
        else:
            logging.debug(f"Robin: {string}")

    def text_to_robot(self, string):
        """Takes human speech and sends it to the self word.
        """
        logging.debug(f"Human: {string}")

        # keep only the word-like characters
        string = re.sub(r'[^\w\s]', '', string)

        for word in string.split():
            actr.new_word_sound(word, self.time)
            self.time += 1  # separate words by 1 second (in model-time)

    def get_human_speech(self):
        """Text to speech from microphone input.
        Returns string.
        """
        recognizer = sr.Recognizer()
        mic = sr.Microphone(device_index=self.mic_index, sample_rate=self.sample_rate)

        with mic as source:
            recognizer.adjust_for_ambient_noise(source)
            logging.debug('[Waiting for human to speak:]')
            audio = recognizer.listen(source)

        # detect speech using engine
        if self.engine == 'sphinx':
            output = recognizer.recognize_sphinx(audio)
        elif self.engine == 'google':
            output = recognizer.recognize_google(audio)

        logging.debug(output)

        return output

    def inner_speech(self):
        """Making Robin start conversation with human using the ACT-R model.
        """
        actr.reset()
        actr.set_parameter_value(":sound-decay-time", 0.3)

        actr.add_command("inner-speech-response", self.robin_speaks,
                        "Inner speech model response")
        actr.monitor_command("output-speech", "inner-speech-response")

        actr.install_device(["speech", "microphone"])

        self.robin_speaks(string='Hi there! [waving hand]')
        
        while True:
            try:
                text = self.get_human_speech()   # retrieves human speech from mic, changes it to a string to be fed to ACT-R 
            except sr.UnknownValueError:
                logging.debug('I did not understand.')
                continue
            self.text_to_robot(text)
            actr.run(self.time+10)
            self.reset_actr()
            if 'goodbye' in text:
                break 


if __name__ == "__main__":
    # uncomment these lines to find mic index
    # for index, name in enumerate(sr.Microphone.list_microphone_names()):
    #     print("Microphone with name \"{1}\" found for `Microphone(device_index={0})`".format(index, name))
    
    mind = Mind(mic_index=0, sample_rate=48000) #, engine='google')
    mind.inner_speech()
