// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
#include <string.h>
#include <libguile.h>
#include <tensorflow/c/c_api.h>
#include "util-helpers.h"


static scm_t_bits tf_tensor_tag;


struct tf_tensor_t {
  TF_Tensor *tensor;
};


static struct tf_tensor_t *get_tf_tensor_no_check(SCM scm_self)
{
  return (struct tf_tensor_t *)SCM_SMOB_DATA(scm_self);
}

static struct tf_tensor_t *get_tf_tensor(SCM scm_self)
{
  scm_assert_smob_type(tf_tensor_tag, scm_self);
  return get_tf_tensor_no_check(scm_self);
}

size_t free_tensor(SCM scm_self)
{
  struct tf_tensor_t *self = get_tf_tensor_no_check(scm_self);
  TF_DeleteTensor(self->tensor);
  scm_gc_free(self, sizeof(struct tf_tensor_t), "tensor");
  return 0;
}

SCM make_tensor(SCM scm_type, SCM scm_shape, SCM scm_size, SCM scm_source)
{
  SCM retval;
  struct tf_tensor_t *self = (struct tf_tensor_t *)scm_gc_calloc(sizeof(struct tf_tensor_t), "make-tensor");
  SCM_NEWSMOB(retval, tf_tensor_tag, self);
  int num_dims = scm_to_int(scm_length(scm_shape));
  int64_t *dims = scm_gc_malloc_pointerless(sizeof(int64_t) * num_dims, "make-tensor");
  for (int i=0; i<num_dims; i++) {
    dims[i] = scm_to_int(scm_car(scm_shape));
    scm_shape = scm_cdr(scm_shape);
  };
  self->tensor = TF_AllocateTensor(scm_to_int(scm_type), dims, num_dims, scm_to_int(scm_size));
  memcpy(TF_TensorData(self->tensor), scm_to_pointer(scm_source), scm_to_int(scm_size));
  return retval;
}

SCM tf_from_tensor(SCM scm_self)
{
  struct tf_tensor_t *self = get_tf_tensor(scm_self);
  int num_dims = TF_NumDims(self->tensor);
  SCM scm_shape = SCM_EOL;
  for (int i=num_dims - 1; i>=0; i--)
    scm_shape = scm_cons(scm_from_int(TF_Dim(self->tensor, i)), scm_shape);
  size_t size = TF_TensorByteSize(self->tensor);
  void *data = scm_gc_malloc_pointerless(size, "from-tensor");
  memcpy(data, TF_TensorData(self->tensor), size);
  return scm_list_3(scm_from_int(TF_TensorType(self->tensor)),
                    scm_shape,
                    scm_from_pointer(data, NULL));
}

void init_tensorflow(void)
{
  tf_tensor_tag = scm_make_smob_type("tensor", sizeof(TF_Tensor *));
  scm_set_smob_free(tf_tensor_tag, free_tensor);

  scm_c_define("TF_UINT8" , scm_from_int(TF_UINT8 ));
  scm_c_define("TF_INT8"  , scm_from_int(TF_INT8  ));
  scm_c_define("TF_UINT16", scm_from_int(TF_UINT16));
  scm_c_define("TF_INT16" , scm_from_int(TF_INT16 ));
  scm_c_define("TF_UINT32", scm_from_int(TF_UINT32));
  scm_c_define("TF_INT32" , scm_from_int(TF_INT32 ));
  scm_c_define("TF_UINT64", scm_from_int(TF_UINT64));
  scm_c_define("TF_INT64" , scm_from_int(TF_INT64 ));
  scm_c_define_gsubr("make-tensor"   , 4, 0, 0, SCM_FUNC(make_tensor));
  scm_c_define_gsubr("tf-from-tensor", 1, 0, 0, SCM_FUNC(tf_from_tensor));
}
