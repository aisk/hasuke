# ハスケ(Hasuke)

> A simple, lightweight AI Agent written in Haskell.

## 🛠️ Installation

```bash
# Build with Stack
stack build

# Set your Anthropic API key
export ANTHROPIC_API_KEY=...

# Set different LLM provider
export ANTHROPIC_BASE_URL=https://...

# Run
stack run
```

> **Note**: You can also use `ANTHROPIC_AUTH_TOKEN` for authentication.

## 📁 Project Structure

```
hasuke/
├── app/Main.hs      -- Entry point
├── src/
│   ├── Types.hs     -- Core type definitions
│   ├── Tool.hs      -- Tool registry & built-in tools
│   ├── Provider.hs  -- LLM Provider interface (Anthropic)
│   └── Agent.hs     -- Agent core logic & turn handling
└── hasuke.cabal     -- Package config
```

## 📝 License

MIT
