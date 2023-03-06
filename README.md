# PCS3556
> 🤔 Repositórios de exercícios programas da diciplina PCS3556 - Lógica Computacional (2023)

> Os exercícios programas vão ser desenvolvidos em clojure

# 🚀 Como executar

Criei e execute o container docker, que já tem tudo instalado:
```sh
chmod +x run_docker.sh
./run_deploy
```

Agora, dentro do container, basta seguir os seguintes comandos

Para executar o script do ep1:
```sh
cd ep1
lein run
```

Para executar o script do ep2:
```sh
cd ep2
lein run
```

Para executar o script do ep2:
```sh
cd ep3
lein run
```

# 🧪 Testes

Execute o container docker, que já tem tudo instalado:
```sh
chmod +x run_docker.sh
./run_deploy
```

Agora, dentro do container, basta seguir os seguintes comandos

Para executar o teste do ep1:
```sh
cd ep1
lein test
```

Para executar o teste do ep2:
```sh
cd ep2
lein test
```

Para executar o teste do ep2:
```sh
cd ep3
lein test
```


# 📦 Dependencias

## Ubuntu

Docker:
https://docs.docker.com/engine/install/ubuntu/

Docker-Compose:
```sh
sudo curl -L "https://github.com/docker/compose/releases/download/1.26.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose

sudo chmod +x /usr/local/bin/docker-compose
```