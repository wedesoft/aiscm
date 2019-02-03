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
Word boundaries are detected using a rising and falling threshold.
The labels are stored as a CSV file.
The audio data is stored in a single MP3 file.

```Scheme
@../tests/integration/voice-record.scm@
```

The training program is shown below.
The program trains a sequence-to-one *LSTM* to classify words.
Note that the training takes several hours on a CPU.
At the end assignment instructions are created "freezing" the model.

```Scheme
@../tests/integration/voice-train.scm@
```

The "frozen" model then can be loaded and applied to real-time audio data as follows.

```Scheme
@../tests/integration/voice-run.scm@
```
