use leptos::*;
use leptos_router::*;
use thaw::*;

#[component]
pub fn Docs() -> impl IntoView {
    view! {
        <div class="flex h-full">
            <div
                class="fixed w-80 p-5 bottom-0 overflow-y-auto text-lg border-r border-r-neutral-300 dark:border-r-neutral-700"
                style="top: 68px;"
            >
                <Space vertical=true gap=SpaceGap::Size(8)>
                    <DocLink uri="/docs" title="Getting Started"/>
                    <DocLink uri="/docs/compiler-design" title="Compiler Design"/>
                </Space>
            </div>
            <div class="ml-80 p-5">
                <Outlet/>
            </div>
        </div>
    }
}

#[component]
pub fn GettingStarted() -> impl IntoView {
    view! { <Markdown source=include_str!("../../docs/getting-started.md")/> }
}

#[component]
pub fn CompilerDesign() -> impl IntoView {
    view! { <Markdown source=include_str!("../../docs/compiler-design.md")/> }
}

#[component]
fn Markdown(source: &'static str) -> impl IntoView {
    let blocks = markdown::tokenize(source);
    view! { <MdBlocks blocks=blocks/> }
}

#[component]
fn MdListItems(items: Vec<markdown::ListItem>) -> impl IntoView {
    items
        .into_iter()
        .map(|item| view! { <MdListItem item=item/> })
        .collect::<Vec<_>>()
}

#[component]
fn MdBlocks(blocks: Vec<markdown::Block>) -> impl IntoView {
    blocks
        .into_iter()
        .map(|block| view! { <MdBlock block=block/> })
        .collect::<Vec<_>>()
}

#[component]
fn MdSpans(spans: Vec<markdown::Span>) -> impl IntoView {
    spans
        .into_iter()
        .map(|span| view! { <MdSpan span=span/> })
        .collect::<Vec<_>>()
}

#[component]
fn MdListItem(item: markdown::ListItem) -> impl IntoView {
    match item {
        markdown::ListItem::Simple(spans) => view! {
            <li class="list-inside">
                <MdSpans spans=spans/>
            </li>
        }
        .into_view(),
        markdown::ListItem::Paragraph(blocks) => view! {
            <li class="list-inside">
                <MdBlocks blocks=blocks/>
            </li>
        }
        .into_view(),
    }
}

#[component]
fn MdBlock(block: markdown::Block) -> impl IntoView {
    match block {
        markdown::Block::Header(spans, level) => {
            let id = &spans
                .iter()
                .filter_map(|span| match span {
                    markdown::Span::Text(text) => Some(text.clone()),
                    _ => None,
                })
                .collect::<String>()
                .to_lowercase()
                .replace(' ', "-");

            match level {
                1 => view! {
                    <h1 class="text-5xl mb-8" id=id>
                        <a href="">
                            <MdSpans spans=spans/>
                        </a>
                    </h1>
                }
                .into_view(),
                2 => view! {
                    <h2 class="text-4xl" id=id style="margin-top: 45px">
                        <a href=format!("#{id}")>
                            <MdSpans spans=spans/>
                        </a>
                    </h2>
                }
                .into_view(),
                3 => view! {
                    <h3 class="text-3xl mt-7" id=id>
                        <a href=format!("#{id}")>
                            <MdSpans spans=spans/>
                        </a>
                    </h3>
                }
                .into_view(),
                4 => view! {
                    <h4 class="text-2xl">
                        <MdSpans spans=spans/>
                    </h4>
                }
                .into_view(),
                5 => view! {
                    <h5 class="text-xl">
                        <MdSpans spans=spans/>
                    </h5>
                }
                .into_view(),
                6 => view! {
                    <h6 class="text-lg">
                        <MdSpans spans=spans/>
                    </h6>
                }
                .into_view(),
                _ => todo!(),
            }
        }
        markdown::Block::Paragraph(spans) => view! {
            <p class="text-lg mt-4">
                <MdSpans spans=spans/>
            </p>
        }
        .into_view(),
        markdown::Block::Blockquote(_blocks) => todo!(),
        markdown::Block::CodeBlock(_lang, _text) => todo!(),
        markdown::Block::OrderedList(_items, _ty) => todo!(),
        markdown::Block::UnorderedList(items) => view! {
            <ul class="text-lg mt-4 ml-4 list-disc">
                <MdListItems items=items/>
            </ul>
        }
        .into_view(),
        markdown::Block::Raw(_text) => todo!(),
        markdown::Block::Hr => todo!(),
    }
}

#[component]
fn MdSpan(span: markdown::Span) -> impl IntoView {
    match span {
        markdown::Span::Break => todo!(),
        markdown::Span::Text(text) => text.into_view(),
        markdown::Span::Code(_text) => todo!(),
        markdown::Span::Link(text, uri, _title) => view! {
            <a
                href=uri.clone()
                target=if uri.starts_with('#') { "_self" } else { "_blank" }
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
                {text}
            </a>
        }
        .into_view(),
        markdown::Span::Image(_text, _uri, _title) => todo!(),
        markdown::Span::Emphasis(_spans) => todo!(),
        markdown::Span::Strong(_spans) => todo!(),
    }
}

#[component]
fn DocLink(uri: &'static str, title: &'static str) -> impl IntoView {
    view! {
        <A href=uri class="block">
            {title}
        </A>
    }
}

#[component]
fn Body(children: Children) -> impl IntoView {
    view! { <p class="mt-8 text-lg">{children()}</p> }
}
