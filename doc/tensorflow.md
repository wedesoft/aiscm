# Tensorflow bindings
## Basic XOR example

The following example demonstrates the *Tensorflow* bindings using the XOR function as example.
The input *features* and the desired output *labels* are provided using placeholder values.
The network is trained using gradient descent.
Finally the network is demonstrated using the input features.

```Scheme
@../tests/integration/xor.scm@
```

## MNIST example

The MNIST dataset is a benchmark dataset for handwritten digit recognition.

![mnist.jpg](mnist.jpg "MNIST data set")

The following example is a convolutional neural network achieving an error rate below 3%.

```Scheme
@../tests/integration/mnist.scm@
```

## Word recognition

The following code is for recording audio training data.
The user has to speak the indicated word.
Word boundaries are set by pressing return.
The labels are stored as part of the file name.
The audio data is stored in multiple MP3 files.

```Scheme
@../tests/integration/speech-record.scm@
```

Furthermore background noise (e.g. mobile robot driving) is recorded using the following code.

```Scheme
@../tests/integration/speech-background.scm@
```

The training program is shown below.
The program trains a sequence-to-sequence *GRU* to classify words.
Note that the training takes one hour on a CPU.
At the end assignment instructions are created "freezing" the model.

```Scheme
@../tests/integration/speech-train.scm@
```

The "frozen" model then can be loaded and applied to real-time audio data as follows.

```Scheme
@../tests/integration/speech-run.scm@
```
