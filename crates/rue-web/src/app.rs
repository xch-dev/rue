use itertools::Itertools;
use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use leptos_use::{storage::use_local_storage, utils::FromToStringCodec};
use rue_lexer::{Lexer, Token, TokenKind};
use rue_web_derive::docs;
use thaw::*;

docs!("docs.json");

#[component]
pub fn App() -> impl IntoView {
    provide_meta_context();

    let (dark, set_dark, _) = use_local_storage::<bool, FromToStringCodec>("dark-mode");

    view! {
        <Title text="Rue Lang"/>
        <Stylesheet id="leptos" href="/pkg/rue-web.css"/>
        <Link rel="shortcut icon" type_="image/ico" href="/favicon.ico"/>
        <Meta charset="UTF-8"/>
        <Meta name="viewport" content="width=device-width, initial-scale=1.0"/>
        <Script blocking="render">{include_str!("../prerender.js")}</Script>

        <Html class=move || if dark.get() { "dark" } else { "" } lang="en"/>
        <Body class="bg-neutral-100 dark:bg-neutral-900 text-gray-800 dark:text-gray-200"/>

        <Router>
            <div
                class="fixed bg-neutral-200 dark:bg-neutral-800 w-full p-3 pl-4 pr-4 z-10"
                style="height: 68px;"
            >
                <Space align=SpaceAlign::Center justify=SpaceJustify::SpaceBetween>
                    <a href="/" class="text-inherit">
                        <Space align=SpaceAlign::Center>
                            <img src="/logo.png" alt="Rue Lang" style="height: 34px;"/>
                            <h1 style="font-size: 30px;" translate="no" class="notranslate">
                                "Rue Lang"
                            </h1>
                        </Space>
                    </a>
                    <Space gap=SpaceGap::Size(30)>
                        <DarkSwitcher dark=dark set_dark=set_dark/>
                        <a href="/docs" class="text-inherit">
                            <Icon icon=icondata::FaBookSolid width="32px" height="32px"/>
                        </a>
                        <a
                            href="https://github.com/rigidity/rue"
                            target="_blank"
                            class="text-inherit"
                        >
                            <Icon icon=icondata::FaGithubBrands width="32px" height="32px"/>
                        </a>
                    </Space>
                </Space>
            </div>

            <div class="absolute w-full" style="top: 68px;">
                <AppRoutes/>
            </div>
        </Router>
    }
}

#[component]
fn AppRoutes() -> impl IntoView {
    view! {
        <Routes>
            <Route path="/" view=Home/>
            <DocsRoutes/>
        </Routes>
    }
}

#[component]
pub fn Docs() -> impl IntoView {
    view! {
        <div class="flex h-full">
            <div
                class="fixed w-80 p-5 bottom-0 overflow-y-auto text-lg border-r border-r-neutral-300 dark:border-r-neutral-700"
                style="top: 68px;"
            >
                <DocsSidebar/>
            </div>
            <div class="ml-80 p-5" style="width: calc(100% - 320px)">
                <Outlet/>
            </div>
        </div>
    }
}

#[component]
fn Category(label: &'static str, children: Children) -> impl IntoView {
    let (get_expanded, set_expended) = create_signal(false);

    view! {
        <div>
            <div
                class="flex justify-between items-center cursor-pointer select-none"
                on:click=move |_| set_expended.set(!get_expanded.get())
            >
                <p class="text-xl">{label}</p>
                {move || {
                    if get_expanded.get() {
                        view! {
                            <Icon icon=icondata::FaChevronDownSolid width="20px" height="20px"/>
                        }
                    } else {
                        view! {
                            <Icon icon=icondata::FaChevronRightSolid width="20px" height="20px"/>
                        }
                    }
                }}

            </div>
            <div class=format!(
                "ml-7 mt-2 flex flex-col gap-2{}",
                if get_expanded.get() { " hidden" } else { "" },
            )>{children()}</div>
        </div>
    }
}

#[component]
fn PageLink(uri: &'static str, label: &'static str) -> impl IntoView {
    view! {
        <A href=uri class="block text-xl">
            {label}
        </A>
    }
}

#[component]
fn DarkSwitcher(dark: Signal<bool>, set_dark: WriteSignal<bool>) -> impl IntoView {
    view! {
        {move || {
            if dark.get() {
                view! {
                    <Icon
                        icon=icondata::BsSunFill
                        width="32px"
                        height="32px"
                        class="cursor-pointer"
                        on:click=move |_| set_dark.set(false)
                    />
                }
            } else {
                view! {
                    <Icon
                        icon=icondata::BsMoonStarsFill
                        width="32px"
                        height="32px"
                        class="cursor-pointer"
                        on:click=move |_| set_dark.set(true)
                    />
                }
            }
        }}
    }
}

#[component]
fn Home() -> impl IntoView {
    view! {
        <div class="mt-20 mx-auto w-11/12 md:w-9/12 lg:w-7/12 xl:w-6/12">
            <h2 class="text-7xl">"Rue"</h2>
            <p class="text-2xl mt-5">
                "A language crafted from the ground up to make developing "
                "smart coin logic on the Chia blockchain easy for everyone."
            </p>
            <a href="/docs">
                <Button class="text-lg mt-5" size=ButtonSize::Large>
                    "Get Started"
                </Button>
            </a>
        </div>
    }
}

#[component]
fn Rue(source: &'static str) -> impl IntoView {
    let tokens = Lexer::new(source).collect_vec();
    let mut output = Vec::new();
    let mut pos = 0;
    let mut brackets = Vec::new();

    for (i, token) in tokens.iter().enumerate() {
        // Make Tailwind completions happy.
        #[allow(clippy::needless_late_init)]
        let mut class;
        let text = source[pos..pos + token.len()].to_string();

        match token.kind() {
            TokenKind::Ident
                if tokens
                    .iter()
                    .skip(i + 1)
                    .find(|token| !token.kind().is_trivia())
                    .map(Token::kind)
                    == Some(TokenKind::OpenParen) =>
            {
                class = "text-blue-600 dark:text-blue-300"
            }
            TokenKind::Ident if text.chars().next().unwrap_or('\0').is_uppercase() => {
                class = "text-green-700 dark:text-yellow-200"
            }
            TokenKind::Ident => class = "text-gray-700 dark:text-red-400",
            TokenKind::Int => class = "text-fuchsia-600 dark:text-green-300",
            TokenKind::String { .. } => class = "text-cyan-600 dark:text-lime-300",
            TokenKind::OpenParen
            | TokenKind::CloseParen
            | TokenKind::OpenBracket
            | TokenKind::CloseBracket
            | TokenKind::OpenBrace
            | TokenKind::CloseBrace => {
                if matches!(
                    token.kind(),
                    TokenKind::OpenParen | TokenKind::OpenBracket | TokenKind::OpenBrace
                ) {
                    brackets.push(token.kind());
                }

                match brackets.len() % 3 {
                    0 => class = "text-pink-700 dark:text-orange-300",
                    1 => class = "text-teal-600 dark:text-green-400",
                    2 => class = "text-sky-600 dark:text-indigo-300",
                    _ => unreachable!(),
                }

                let mut invalid_bracket = false;

                if matches!(
                    token.kind(),
                    TokenKind::CloseParen | TokenKind::CloseBracket | TokenKind::CloseBrace
                ) {
                    let previous = brackets.last().copied();

                    match token.kind() {
                        TokenKind::CloseParen if previous != Some(TokenKind::OpenParen) => {
                            invalid_bracket = true;
                        }
                        TokenKind::CloseBracket if previous != Some(TokenKind::OpenBracket) => {
                            invalid_bracket = true;
                        }
                        TokenKind::CloseBrace if previous != Some(TokenKind::OpenBrace) => {
                            invalid_bracket = true;
                        }
                        _ if brackets.pop().is_none() => {
                            invalid_bracket = true;
                        }
                        _ => {}
                    }
                }

                if invalid_bracket {
                    class = "text-red-600 dark:text-red-500";
                }
            }
            TokenKind::Fun
            | TokenKind::Inline
            | TokenKind::Type
            | TokenKind::Struct
            | TokenKind::Enum
            | TokenKind::Let
            | TokenKind::Const
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Return
            | TokenKind::Raise
            | TokenKind::Assert
            | TokenKind::As
            | TokenKind::Is => class = "text-purple-700 dark:text-purple-400",
            TokenKind::Nil | TokenKind::True | TokenKind::False => {
                class = "text-orange-700 dark:text-orange-300"
            }
            TokenKind::Dot
            | TokenKind::Comma
            | TokenKind::Colon
            | TokenKind::PathSeparator
            | TokenKind::Semicolon
            | TokenKind::Arrow
            | TokenKind::FatArrow
            | TokenKind::Spread
            | TokenKind::Question
            | TokenKind::Assign => class = "text-gray-700 dark:text-gray-300",
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Not
            | TokenKind::LessThan
            | TokenKind::GreaterThan
            | TokenKind::LessThanEquals
            | TokenKind::GreaterThanEquals
            | TokenKind::Equals
            | TokenKind::NotEquals => class = "text-blue-700 dark:text-blue-300",
            TokenKind::Whitespace => class = "",
            TokenKind::LineComment | TokenKind::BlockComment { .. } => {
                class = "text-gray-500 dark:text-gray-400"
            }
            TokenKind::Unknown => class = "text-red-600 dark:text-red-500",
        };

        pos += token.len();

        output.push(view! { <span class=class>{text}</span> });
    }

    output
}
