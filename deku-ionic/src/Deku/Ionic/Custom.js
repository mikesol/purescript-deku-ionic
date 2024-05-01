export const customComponentImpl =
    (componentName) => (localProps) => (connectedHook) => (disconnectedHook) => (run) => () => {
    class CustomComponent extends HTMLElement {
      constructor() {
        super();
        this.ionic$locals = undefined;
      }
      disconnectedCallback() {
        disconnectedHook(this.ionic$locals)();
      }
      connectedCallback() {
        this.ionic$locals = {};
        for (var i = 0; i < localProps.length; i++) {
          this.ionic$locals[localProps[i]] = this[localProps[i]];
        }
        run(this)(this.ionic$locals)();
        connectedHook(this.ionic$locals)();
      }
    }
    window.customElements.define(componentName, CustomComponent);
  };
