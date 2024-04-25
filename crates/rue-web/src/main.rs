use leptos::*;
use leptos_meta::Html;
use leptos_router::*;
use leptos_use::{storage::use_local_storage, utils::FromToStringCodec};
use thaw::*;

mod docs;

use crate::docs::*;

fn main() {
    mount_to_body(App)
}

#[component]
pub fn App() -> impl IntoView {
    view! {
        <Router>
            <Header/>
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
            <Route path="/docs" view=Docs>
                <Route path="" view=GettingStarted/>
                <Route path="compiler-design" view=CompilerDesign/>
            </Route>
        </Routes>
    }
}

fn dark_switcher(dark: Signal<bool>, set_dark: WriteSignal<bool>) -> impl IntoView {
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
}

#[component]
fn Header() -> impl IntoView {
    let (dark, set_dark, _) = use_local_storage::<bool, FromToStringCodec>("dark-mode");

    view! {
        <div
            class="fixed bg-neutral-200 dark:bg-neutral-800 w-full p-3 pl-4 pr-4"
            style="height: 68px;"
        >
            <Html class=move || if dark.get() { "dark" } else { "" }/>
            <Space align=SpaceAlign::Center justify=SpaceJustify::SpaceBetween>
                <a href="/" class="text-inherit">
                    <Space align=SpaceAlign::Center>
                        <img src="/logo.png" alt="Rue Lang" style="height: 34px;"/>
                        <h1 style="font-size: 30px;" translate="no" class="notranslate">
                            "Rue Lang"
                        </h1>
                    </Space>
                </a>
                <Space gap=SpaceGap::Size(
                    30,
                )>
                    {move || { dark_switcher(dark, set_dark) }}
                    <a href="/docs" class="text-inherit">
                        <Icon icon=icondata::FaBookSolid width="32px" height="32px"/>
                    </a>
                    <a href="https://github.com/rigidity/rue" target="_blank" class="text-inherit">
                        <Icon icon=icondata::FaGithubBrands width="32px" height="32px"/>
                    </a>
                </Space>
            </Space>
        </div>
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
