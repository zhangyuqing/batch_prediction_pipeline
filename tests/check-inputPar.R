length(command_args) >= 16

n_genes
n_on

on_mean
off_mean
on_var
off_var
on_var==off_var

n_sample_train
n_case_train
n_control_train

n_sample_test
n_case_test
n_control_test


while(withBatch){print("YES!"); break}
batch_meanvar_arg
sep_cmb
combat_mod


n_batch_train
n_batch_test
n_batch_train==1 || n_batch_test==1

length(command_args) == 16+2*(n_batch_train+n_batch_test)

length(controls_batch_test) == n_batch_test
length(controls_batch_train) == n_batch_train

length(command_args) > 17
new_args

cases_batch_train
controls_batch_train

cases_batch_test
controls_batch_test

n_batch_train > 0 & (sum(cases_batch_train)!=n_case_train)
n_batch_train > 0 & (sum(controls_batch_train)!=n_control_train)
n_batch_test > 0 & (sum(cases_batch_test)!=n_case_test)
n_batch_test > 0 & (sum(controls_batch_test)!=n_control_test)

iterations
