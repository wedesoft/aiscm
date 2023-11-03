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
