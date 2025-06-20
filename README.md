
# 📚 jp_bibrev

Ferramentas em R para revisão e organização de referências bibliográficas.

---

## 🚩 Sobre o projeto

Este repositório reúne funções desenvolvidas em R para:

- 🔍 Buscar referências automaticamente a partir de palavras-chave  
- 📑 Filtrar, limpar e organizar bases bibliográficas  
- 📝 Exportar os resultados em formatos prontos para análise ou inclusão em textos científicos  

As funções aqui disponibilizadas foram criadas para apoiar um dos capítulos da tese de doutorado de **Julia Pierry**, vinculada à **Universidade Federal do Paraná (UFPR)**.

---

## 🚀 Funcionalidades principais

✅ Busca de referências por termos específicos  
✅ Padronização de campos como autores, título e revista  
✅ Marcação de relevância dos artigos (manual ou automatizada)  
✅ Exportação de tabelas limpas para Excel ou CSV  
✅ Geração de planilhas para acompanhamento da revisão  

---

## 🛠️ Estrutura do repositório

```
jp_bibrev/
├── R/               # Scripts com as funções
├── data/            # Bases de referências em diferentes etapas de processamento
├── README.md        # Este arquivo
```

---

## 📦 Exemplos de funções

- `jp_busca()`: Automatiza a busca inicial por palavras-chave  
- `jp_limpa()`: Organiza e padroniza os campos da base  
- `jp_exporta()`: Gera arquivos finais para exportação e uso posterior  

*(O conjunto de funções pode ser expandido conforme o avanço da pesquisa.)*

---

## 🤓 Público-alvo

👉 Pesquisadores  
👉 Estudantes de pós-graduação  
👉 Equipes envolvidas em revisões sistemáticas ou bibliográficas

---

## 📌 Exemplo de uso

```r
source("R/jp_busca.R")
referencias <- jp_busca(termos = c("estuarine dolphins", "habitat use"))

source("R/jp_limpa.R")
referencias_limpa <- jp_limpa(referencias)

source("R/jp_exporta.R")
jp_exporta(referencias_limpa, file = "data/referencias_tratadas.xlsx")
```

---

## 📄 Licença

Distribuído sob a licença MIT 📝
