---
title: Building a RAG System in Go
date: 2026-04-02
tags:
  - go
  - ai
  - rag
  - postgres
---

I wanted to ask questions against my own documents without sending them to any third party vendor. So I built a small RAG system in Go.

You POST a text file to it, it chunks and embeds it with [nomic-embed-text](https://ollama.com/library/nomic-embed-text) via Ollama, and stores the vectors in Postgres using [pgvector](https://github.com/pgvector/pgvector). When you ask a question, it embeds that too, finds the closest chunks, and sends them as context to llama3. Everything runs locally.

```bash
curl -X POST http://localhost:8080/documents -F "file=@notes.txt"
curl -X POST http://localhost:8080/query \
  -H "Content-Type: application/json" \
  -d '{"question": "What does the document say about X?"}'
```

The part that surprised me most was how little pgvector adds on top of plain Postgres — it really is just an extension. I expected something more exotic. The harder problem was chunking: split too coarsely and the retrieved context is noisy, too finely and you lose meaning. I haven't really solved that yet.

Still a work in progress. Source on [GitHub](https://github.com/moulindavid/rag).
