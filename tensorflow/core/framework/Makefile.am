EXTRA_DIST = attr_value.proto op_def.proto resource_handle.proto tensor.proto tensor_shape.proto types.proto

BUILT_SOURCES = attr_value.pb-c.h op_def.pb-c.h resource_handle.pb-c.h tensor.pb-c.h tensor_shape.pb-c.h types.pb-c.h \
								attr_value.pb-c.c op_def.pb-c.c resource_handle.pb-c.c tensor.pb-c.c tensor_shape.pb-c.c types.pb-c.c

%.pb-c.c %.pb-c.h: %.proto
	cd ../../.. && $(PROTOCC) tensorflow/core/framework/$< --c_out=. && cd tensorflow/core/framework

noinst_LTLIBRARIES = libtensorflow-ops.la

libtensorflow_ops_la_SOURCES = attr_value.pb-c.c op_def.pb-c.c resource_handle.pb-c.c tensor.pb-c.c tensor_shape.pb-c.c types.pb-c.c
libtensorflow_ops_la_CFLAGS =
libtensorflow_ops_la_LIBADD =
