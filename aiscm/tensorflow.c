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

SCM make_tensor(SCM scm_type)
{
  SCM retval;
  struct tf_tensor_t *self = (struct tf_tensor_t *)scm_gc_calloc(sizeof(struct tf_tensor_t), "tensor");
  SCM_NEWSMOB(retval, tf_tensor_tag, self);
  void *data = malloc(1);
  self->tensor = TF_AllocateTensor(scm_to_int(scm_type), NULL, 0, 4);
  return retval;
}

SCM tf_from_tensor(SCM scm_self)
{
  struct tf_tensor_t *self = get_tf_tensor(scm_self);
  return scm_from_int(TF_TensorType(self->tensor));
}

void init_tensorflow(void)
{
  tf_tensor_tag = scm_make_smob_type("tensor", sizeof(TF_Tensor *));
  scm_set_smob_free(tf_tensor_tag, free_tensor);

  scm_c_define("TF_UINT8", scm_from_int(TF_UINT8));
  scm_c_define("TF_INT16", scm_from_int(TF_INT16));
  scm_c_define("TF_INT32", scm_from_int(TF_INT32));
  scm_c_define_gsubr("make-tensor"   , 1, 0, 0, SCM_FUNC(make_tensor));
  scm_c_define_gsubr("tf-from-tensor", 1, 0, 0, SCM_FUNC(tf_from_tensor));
}
