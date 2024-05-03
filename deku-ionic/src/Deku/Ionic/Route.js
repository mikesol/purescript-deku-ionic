export const beforeEnter = (router) => (cb) => () => {
  router.beforeEnter = cb;
};
export const beforeLeave = (router) => (cb) => () => {
  router.beforeLeave = cb;
};
export const componentProps = (router) => (props) => () => {
  router.componentProps = props;
};
export const getBeforeEnter = (router) => () => router.beforeEnter;
export const getBeforeLeave = (router) => () => router.beforeLeave;
export const getComponentProps = (router) => () => router.componentProps;

//////
export const unsafeCustomComponentImpl =
  (componentName) => (connectedHook) => (disconnectedHook) => (run) => () => {
    console.log('cn', componentName);
    class CustomComponent extends HTMLElement {
      constructor() {
        super();
      }
      disconnectedCallback() {
        disconnectedHook();
      }
      connectedCallback() {
        const params = new URLSearchParams(window.location.search);
        const qValue = params.get("q");
        const decodedValue = decodeURIComponent(qValue);
        run(this)(decodedValue)();
        connectedHook();
      }
    }
    window.customElements.define(componentName, CustomComponent);
  };
