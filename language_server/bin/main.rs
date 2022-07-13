use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::vec;

use dashmap::DashMap;
// use nrs_language_server::chumsky::{parse, type_inference, Func, ImCompleteSemanticToken, Spanned};
// use nrs_language_server::completion::completion;
// use nrs_language_server::jump_definition::{get_definition, get_definition_of_expr};
// use nrs_language_server::reference::get_reference;
use lc3_language_server::semantic_token;
use ropey::Rope;
// use serde::{Deserialize, Serialize};
// use serde_json::Value;
use tower_lsp::jsonrpc::{ErrorCode, Result};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, lsp_types, LspService, Server};
use lc3_assembler::{Assembly, LeniencyLevel, parse_and_analyze, Parsed, Validated};
use lc3_assembler::error::Error;
use crate::request::{GotoDeclarationParams, GotoDeclarationResponse, GotoImplementationParams, GotoImplementationResponse, GotoTypeDefinitionParams, GotoTypeDefinitionResponse};

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: DashMap<String, Rope>,
    semantic_token_map: DashMap<String, Vec<Spanned<semantic_token::SemanticToken>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                // TODO: Completion? (completion_provider)
                // TODO: Commands? (execute_command_provider)
                // TODO: Workspaces, linking (workspace)
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("lc3".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: semantic_token::legend(),
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                // TODO: definition_provider, references_provider, rename_provider (at least for labels)
                ..ServerCapabilities::default()
            },
        })
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_tokens_full")
            .await;
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let simple_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            semantic_token::semantic_tokens(&simple_tokens, &rope)
        }();
        Ok(if let Some(semantic_token) = semantic_tokens {
            Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            }))
        } else {
            None
        })
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_tokens_full")
            .await;
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let simple_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            semantic_token::semantic_tokens(&simple_tokens, &rope)
        }();
        self.client
            .log_message(MessageType::INFO, &format!("{:?}", semantic_tokens))
            .await;
        Ok(if let Some(semantic_token) = semantic_tokens {
            Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            }))
        } else {
            None
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(params.text_document.into())
            .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
            .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl From<lsp_types::TextDocumentItem> for TextDocumentItem {
    fn from(tdi: lsp_types::TextDocumentItem) -> Self {
        Self {
            uri: tdi.uri,
            text: tdi.text,
            version: tdi.version
        }
    }
}

fn diagnostics(error: &Error, rope: &Rope) -> Vec<Diagnostic> {
    match error {
        Error::Single(_, e) => vec![],
        Error::Spanned(span, e) => {
            let diagnostic = || -> Option<Diagnostic> {
                let start_position = offset_to_position(span.start(), &rope)?;
                let end_position = offset_to_position(span.end(), &rope)?;
                Some(Diagnostic::new_simple(
                    Range::new(start_position, end_position),
                    e.message(),
                ))
            }();
            diagnostic.into_iter().collect()
        }
        Error::Multiple(es) =>
            es.iter()
              .flat_map(|e| diagnostics(e, rope))
              .collect()
    }
}

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

fn parse(id: lc3_assembler::SourceId, src: String) -> std::result::Result<(Spanned<lc3_assembler::parse::File>, Error), Error> {
    Ok(lc3_assembler::Assembly::<String>::new(Some(id), src, LeniencyLevel::Lenient)
        .lex()?
        .parse()?
        .file_and_errors())
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let rope = Rope::from_str(&params.text);
        self.document_map.insert(params.uri.to_string(), rope.clone());
        let id = params.uri.to_file_path()
            .map(|p| lc3_assembler::id(&p))
            .unwrap_or(lc3_assembler::dummy_id());


        let (semantic_tokens, diagnostics) =
            match parse(id, params.text) {
                Ok((file, e)) => {
                    let semantic_tokens = semantic_token::simple_semantic_tokens(&file);
                    (semantic_tokens, diagnostics(&e, &rope))
                },
                Err(e) => (vec![], diagnostics(&e, &rope)),
            };

         self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;

        self.client
            .log_message(MessageType::INFO, &format!("{:?}", semantic_tokens))
            .await;
        self.semantic_token_map.insert(params.uri.to_string(), semantic_tokens);
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) =
        LspService::build(|client|
            Backend {
                client,
                document_map: Default::default(),
                semantic_token_map: Default::default(),
            })
        .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}