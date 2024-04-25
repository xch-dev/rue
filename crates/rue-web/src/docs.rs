use leptos::*;
use leptos_router::*;
use thaw::*;

mod compiler_design;
mod getting_started;

pub use compiler_design::*;
pub use getting_started::*;

#[component]
pub fn Docs() -> impl IntoView {
    view! {
        <div class="flex h-full">
            <div
                class="fixed w-80 p-5 bottom-0 overflow-y-auto text-lg border-r border-r-neutral-600"
                style="top: 68px;"
            >
                <Space vertical=true gap=SpaceGap::Size(8)>
                    <DocLink uri="/docs" title="Getting Started"/>
                    <DocLink uri="/docs/compiler-design" title="Compiler Design"/>
                </Space>
            </div>
            <div class="ml-80 p-5 -z-10">
                <Outlet/>
            </div>
        </div>
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
fn PageTitle(id: &'static str, children: Children) -> impl IntoView {
    view! {
        <h2 class="text-5xl" id=id>
            <a href=format!("#{id}")>{children()}</a>
        </h2>
    }
}

#[component]
fn Body(children: Children) -> impl IntoView {
    view! { <p class="mt-8 text-lg">{children()}</p> }
}

#[component]
fn Link(href: String, children: Children) -> impl IntoView {
    view! {
        <a
            href=href
            target="_blank"
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
            {children()}
        </a>
    }
}
