use std::{env, fs, path::PathBuf};

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use serde::Deserialize;
use syn::{parse2, Ident, LitStr};

#[derive(Deserialize)]
struct Docs {
    sidebar_items: Vec<Item>,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum Item {
    Page(PageItem),
    Category(CategoryItem),
}

#[derive(Deserialize)]
struct PageItem {
    label: String,
    uri: String,
    content: String,
}

#[derive(Deserialize)]
struct CategoryItem {
    label: String,
    items: Vec<Item>,
}

#[proc_macro]
pub fn docs(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    docs_impl(input).into()
}

fn docs_impl(input: TokenStream) -> TokenStream {
    let relative_path: LitStr = parse2(input).unwrap();
    let path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join(relative_path.value());
    let text = fs::read_to_string(path.as_path()).unwrap();
    let docs: Docs = serde_json::from_str(&text).unwrap();
    let dir = path.parent().unwrap().join("docs");

    let mut pages = Vec::new();
    flatten_pages(&mut pages, &docs.sidebar_items);

    let mut components = Vec::new();
    let mut routes = Vec::new();

    for (i, page) in pages.into_iter().enumerate() {
        let uri = LitStr::new(page.uri.as_str(), Span::call_site());
        let component = Ident::new(&format!("DocsComponent{i}"), Span::call_site());

        let content_path = dir.join(&page.content);
        let content_path_str = LitStr::new(content_path.to_str().unwrap(), Span::call_site());
        let content_text = fs::read_to_string(content_path.as_path()).unwrap();
        let blocks = markdown::tokenize(&content_text);
        let content = block_tokens(&blocks);

        components.push(quote! {
            #[component]
            fn #component() -> impl IntoView {
                include_str!(#content_path_str);
                view! {
                    #content
                }
            }
        });

        routes.push(quote! {
            <Route path=#uri view=#component />
        });
    }

    let sidebar_tokens = sidebar_tokens(&docs.sidebar_items);

    quote! {
        #( #components )*

        #[component(transparent)]
        fn DocsRoutes() -> impl IntoView {
            view! {
                <Route path="/docs" view=Docs>
                    #( #routes )*
                </Route>
            }
        }

        #[component]
        fn DocsSidebar() -> impl IntoView {
            view! {
                #sidebar_tokens
            }
        }
    }
}

fn sidebar_tokens(items: &[Item]) -> TokenStream {
    let mut body_tokens = TokenStream::new();
    for item in items {
        body_tokens.extend(match item {
            Item::Category(category) => category_tokens(category),
            Item::Page(page) => page_tokens(page),
        });
    }
    body_tokens
}

fn category_tokens(category: &CategoryItem) -> TokenStream {
    let label = LitStr::new(category.label.as_str(), Span::call_site());
    let tokens: Vec<TokenStream> = category
        .items
        .iter()
        .map(|item| match item {
            Item::Category(category) => category_tokens(category),
            Item::Page(page) => page_tokens(page),
        })
        .collect();

    quote! {
        <Category label=#label>
            #( #tokens )*
        </Category>

    }
}

fn page_tokens(page: &PageItem) -> TokenStream {
    let uri = LitStr::new(page.uri.as_str(), Span::call_site());
    let label = LitStr::new(page.label.as_str(), Span::call_site());

    quote! {
        <PageLink uri=#uri label=#label />
    }
}

fn block_tokens(blocks: &[markdown::Block]) -> TokenStream {
    let mut tokens = TokenStream::new();
    for block in blocks {
        tokens.extend(match block {
            markdown::Block::Header(spans, level) => header_tokens(spans, *level),
            markdown::Block::Paragraph(spans) => paragraph_tokens(spans),
            markdown::Block::Blockquote(_blocks) => todo!("block quote"),
            markdown::Block::CodeBlock(lang, text) => code_block_tokens(
                &lang.clone().expect("code block language must be specified"),
                text,
            ),
            markdown::Block::OrderedList(_items, _ty) => todo!("ordered list"),
            markdown::Block::UnorderedList(items) => unordered_list_tokens(items),
            markdown::Block::Raw(_text) => todo!("raw block"),
            markdown::Block::Hr => todo!("horizontal rule"),
        });
    }
    tokens
}

fn header_tokens(spans: &[markdown::Span], level: usize) -> TokenStream {
    let element = Ident::new(&format!("h{}", level), Span::call_site());
    let class = LitStr::new(
        match level {
            1 => "text-5xl mb-8",
            2 => "text-4xl",
            3 => "text-3xl mt-8",
            4 => "text-2xl",
            5 => "text-xl",
            6 => "text-lg",
            _ => unreachable!(),
        },
        Span::call_site(),
    );

    let mut span_tokens = span_tokens(spans);

    if level == 1 {
        span_tokens = quote! {
            <a href="">
                #span_tokens
            </a>
        };
    } else if level <= 3 {
        let id = spans
            .iter()
            .filter_map(|span| match span {
                markdown::Span::Text(text) => Some(text.clone()),
                _ => None,
            })
            .collect::<String>()
            .to_lowercase()
            .replace(' ', "-");

        span_tokens = quote! {
            <a href=#id>
                #span_tokens
            </a>
        };
    }

    let style = match level {
        2 => "margin-top: 45px",
        _ => "",
    };

    quote! {
        <#element class=#class style=#style>
            #span_tokens
        </#element>
    }
}

fn paragraph_tokens(spans: &[markdown::Span]) -> TokenStream {
    let span_tokens = span_tokens(spans);

    quote! {
        <p class="text-lg mt-4">
            #span_tokens
        </p>
    }
}

fn code_block_tokens(lang: &str, text: &str) -> TokenStream {
    let content = match lang {
        "rue" => {
            quote! {
                <Rue source=#text />
            }
        }
        "bash" => LitStr::new(text, Span::call_site()).into_token_stream(),
        _ => unimplemented!("unsupported code block language: {}", lang),
    };

    quote! {
        <span class="block whitespace-pre overflow-x-scroll font-mono mt-2 text-sm bg-gray-200 dark:bg-gray-800 p-2 rounded">
            #content
        </span>
    }
}

fn list_item_tokens(items: &[markdown::ListItem]) -> TokenStream {
    let mut tokens = TokenStream::new();
    for item in items {
        tokens.extend(match item {
            markdown::ListItem::Simple(spans) => {
                let span_tokens = span_tokens(spans);
                quote! {
                    <li class="list-inside">
                        #span_tokens
                    </li>
                }
            }
            markdown::ListItem::Paragraph(blocks) => {
                let block_tokens = block_tokens(blocks);
                quote! {
                    <li class="list-inside">
                        #block_tokens
                    </li>
                }
            }
        });
    }
    tokens
}

fn unordered_list_tokens(items: &[markdown::ListItem]) -> TokenStream {
    let tokens = list_item_tokens(items);

    quote! {
        <ul class="text-lg mt-4 ml-4 list-disc">
            #tokens
        </ul>
    }
}

fn span_tokens(spans: &[markdown::Span]) -> TokenStream {
    let mut tokens = TokenStream::new();
    for span in spans {
        tokens.extend(match span {
            markdown::Span::Break => todo!("line break"),
            markdown::Span::Text(text) => LitStr::new(text, Span::call_site()).into_token_stream(),
            markdown::Span::Code(text) => inline_code_tokens(text),
            markdown::Span::Link(text, uri, _title) => link_tokens(text, uri),
            markdown::Span::Image(_text, _uri, _title) => todo!("image"),
            markdown::Span::Emphasis(_spans) => todo!("emphasis"),
            markdown::Span::Strong(_spans) => todo!("strong"),
        });
    }
    tokens
}

fn inline_code_tokens(text: &str) -> TokenStream {
    quote! {
        <code class="text-sm bg-gray-200 dark:bg-gray-800 p-1 rounded">
            #text
        </code>
    }
}

fn link_tokens(text: &str, uri: &str) -> TokenStream {
    let target = if uri.starts_with('#') {
        LitStr::new("_self", Span::call_site())
    } else {
        LitStr::new("_blank", Span::call_site())
    };

    let text = LitStr::new(text, Span::call_site());
    let uri = LitStr::new(uri, Span::call_site());

    quote! {
        <a
            href=#uri
            target=#target
            class="
                text-blue-600
                hover:text-blue-500
                visited:text-purple-600
                hover:visited:text-purple-500
                dark:text-blue-300
                dark:hover:text-blue-400
                dark:visited:text-purple-300
                dark:hover:visited:text-purple-400
                hover:underline
            "
        >
            #text
        </a>
    }
}

fn flatten_pages<'a>(pages: &mut Vec<&'a PageItem>, items: &'a [Item]) {
    for item in items {
        match item {
            Item::Page(page) => pages.push(page),
            Item::Category(category) => flatten_pages(pages, &category.items),
        }
    }
}
