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

Download this repository and put it into `~/Documents/`.

From a terminal, go to the ACT-R directory and run the software:

    $ ./run-act-r.command

If it fails, it might be that you need to give it executable permissions first:

    $ chmod +x run-act-r.command
 
Back to the repository folder, run the Python code `demo.py`.

Now you can see the model working in your ACT-R console.

You can hear the inner speech dialogue by installing the [SpeechRecognition](https://pypi.org/project/SpeechRecognition/) Python library with:
    
        $ pip install SpeechRecognition

After SpeechRecognition installation, when you run the model you should hear the inner dialogue produced by your machine.

## Running inner speech model on real robot

### Prerequisites

Operative systems:

- Ubuntu 16.04 LTS or newer, or Windows with [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-manual)

- For integrating the framework on Pepper robot, just ROS kinetic version is required (that is the ROS version for MoveIt! library), which runs on Ubuntu 16.04 LTS.

If you will use a different robot model, please verify the suitable ROS version, and the corresponding Ubuntu version.

### 1 Prepare the environment

- Install ACT-R architecture and the inner speech model

Please, follow the instructions of the previous case (running inner speech model (without robot))


- Install MoveIt! (which requires ROS)

This is a [complete guide](http://docs.ros.org/kinetic/api/moveit_tutorials/html/doc/getting_started/getting_started.html) which allows you to install the complete framework for running ROS and enabling robot movements during interaction. Pay attention to the ROS version.

-  Install the configuration file of your robot

You can find or built your own robot model by following the instuctions of the previous step in the section "[Robot model and Robot state](http://docs.ros.org/en/kinetic/api/moveit_tutorials/html/doc/robot_model_and_robot_state/robot_model_and_robot_state_tutorial.html)".

For Pepper, you can use the pepper_moveit_config package available at

<https://github.com/ros-naoqi/pepper_moveit_config>

Follow all the instructions to play with Pepper and try movements.

2 Compile the framework middleware
----------------------------------
The src package doesn't need any compilation so running rospack profile should be enough.

Please, refer to the file demo_isc.py into the src folder to see how implementing new robot movements and behaviours according to the inner turns.

Launch the middleware like this:

    rosrun robot-inner-speech demo_isc.py
    
and see your Pepper robot interacts to you while producing inner speech. 








