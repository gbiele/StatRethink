data {
  int<lower=0> N;
  vector[N] Y;
  matrix[N,2] X;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[4] theta;
}

model {
  theta ~ std_normal();
  Y ~ normal(theta[1] + X * theta[2:3] , exp(theta[4]));
}

